{-# LANGUAGE CPP, BangPatterns, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables, TupleSections #-}

-- |
-- Module:      Data.Configurator
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A simple (yet powerful) library for working with configuration
-- files.
--
-- Note that while the "Data.Configurator.Parser" and
-- "Data.Configurator.FromValue" should be quite stable at this point,
-- this module is likely to be subjected to significant breaking changes in
-- subsequent versions of configurator-ng.   So please do file an issue
-- if you have any opinions or especially needs with regards to
-- configuration (re)loading,  change notifications,  etc.

module Data.Configurator
    (
    -- * Configuration file format
    -- $format

    -- ** Binding a name to a value
    -- $binding

    -- *** Value types
    -- $types

    -- *** String interpolation
    -- $interp

    -- ** Grouping directives
    -- $group

    -- ** Importing files
    -- $import

    -- * Types
      Worth(..)
    -- * Loading configuration data
    , autoReload
    , autoReloadGroups
    , autoConfig
{--
    -- * Lookup functions

    , lookup
    , lookupDefault
    , require
--}
    -- * Notification of configuration changes
    -- $notify
    , prefix
    , exact
    , subscribe
    -- * Low-level loading functions
    , load
    , loadGroups
    , reload
    , addToConfig
    , addGroupsToConfig
    -- * Helper functions
    , display
    , readConfig
    ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<$>))
#endif
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (SomeException, evaluate, handle, throwIO, try)
import Control.Monad (foldM, forM, forM_, when, msum)
import Data.Configurator.Syntax (interp, topLevel)
import Data.Configurator.Types.Internal
import Data.Configurator.Config.Internal(ConfigPlan(ConfigPlan), Config(Config))
import Data.Int (Int64)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Data.List (tails)
import Data.Maybe (isJust)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mconcat)
#endif
import Data.Scientific ( toBoundedInteger, toRealFloat )
import Data.Text.Lazy.Builder (fromString, fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Prelude hiding (lookup)
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (EpochTime, FileOffset)
import System.PosixCompat.Files (fileSize, getFileStatus, modificationTime)
import qualified Control.Exception as E
import qualified Data.Attoparsec.Text as T
import qualified Data.Attoparsec.Text.Lazy as L
import qualified Data.HashMap.Lazy as H
import qualified Data.CritBit.Map.Lazy as CB
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

loadFiles :: [Worth Path] -> IO (H.HashMap (Worth Path) [Directive])
loadFiles = foldM go H.empty
 where
   go seen path = do
     let rewrap n = const n <$> path
         wpath = worth path
     path' <- rewrap <$> interpolate "" wpath CB.empty
     ds    <- loadOne (T.unpack <$> path')
     let !seen'    = H.insert path ds seen
         notSeen n = not . isJust . H.lookup n $ seen
     foldM go seen' . filter notSeen . importsOf wpath $ ds

-- | Create a 'ConfigCache' from the contents of the named files. Throws an
-- exception on error, such as if files do not exist or contain errors.
--
-- File names have any environment variables expanded prior to the
-- first time they are opened, so you can specify a file name such as
-- @\"$(HOME)/myapp.cfg\"@.
load :: [Worth FilePath] -> IO ConfigCache
load files = load' Nothing (map (\f -> ("", f)) files)

-- | Create a 'ConfigCache' from the contents of the named files, placing them
-- into named prefixes.  If a prefix is non-empty, it should end in a
-- dot.
loadGroups :: [(Name, Worth FilePath)] -> IO ConfigCache
loadGroups files = load' Nothing files

load' :: Maybe AutoConfig -> [(Name, Worth FilePath)] -> IO ConfigCache
load' auto paths0 = do
  let second f (x,y) = (x, f y)
      paths          = map (second (fmap T.pack)) paths0
  ds <- loadFiles (map snd paths)
  p <- newIORef paths
  m <- newIORef =<< flatten paths ds
  s <- newIORef H.empty
  return ConfigCache {
                cfgAuto = auto
              , cfgPaths = p
              , cfgMap = m
              , cfgSubs = s
              }

-- | Forcibly reload a 'ConfigCache'. Throws an exception on error, such as
-- if files no longer exist or contain errors.
reload :: ConfigCache -> IO ()
reload cfg@ConfigCache{..} = do
  paths <- readIORef cfgPaths
  m' <- flatten paths =<< loadFiles (map snd paths)
  m <- atomicModifyIORef cfgMap $ \m -> (m', m)
  notifySubscribers cfg m m' =<< readIORef cfgSubs

-- | Add additional files to a 'ConfigCache', causing it to be reloaded to add
-- their contents.
addToConfig :: [Worth FilePath] -> ConfigCache -> IO ()
addToConfig paths0 cfg = addGroupsToConfig (map (\x -> ("",x)) paths0) cfg

-- | Add additional files to named groups in a 'ConfigCache', causing it to be
-- reloaded to add their contents.  If the prefixes are non-empty, they should
-- end in dots.
addGroupsToConfig :: [(Name, Worth FilePath)] -> ConfigCache -> IO ()
addGroupsToConfig paths0 cfg@ConfigCache{..} = do
  let fix (x,y) = (x, fmap T.pack y)
      paths     = map fix paths0
  atomicModifyIORef cfgPaths $ \prev -> (prev ++ paths, ())
  reload cfg

-- | Defaults for automatic 'Config' reloading when using
-- 'autoReload'.  The 'interval' is one second, while the 'onError'
-- action ignores its argument and does nothing.
autoConfig :: AutoConfig
autoConfig = AutoConfig {
               interval = 1
             , onError = const $ return ()
             }

-- | Load a 'ConfigCache' from the given 'FilePath's, and start a reload
-- thread.
--
-- At intervals, a thread checks for modifications to both the
-- original files and any files they refer to in @import@ directives,
-- and reloads the 'ConfigCache' if any files have been modified.
--
-- If the initial attempt to load the configuration files fails, an
-- exception is thrown.  If the initial load succeeds, but a
-- subsequent attempt fails, the 'onError' handler is invoked.
--
-- File names have any environment variables expanded prior to the
-- first time they are opened, so you can specify a file name such as
-- @\"$(HOME)/myapp.cfg\"@.
autoReload :: AutoConfig
           -- ^ Directions for when to reload and how to handle
           -- errors.
           -> [Worth FilePath]
           -- ^ Configuration files to load.
           -> IO (ConfigCache, ThreadId)
autoReload auto paths = autoReloadGroups auto (map (\x -> ("", x)) paths)

autoReloadGroups :: AutoConfig
                 -> [(Name, Worth FilePath)]
                 -> IO (ConfigCache, ThreadId)
autoReloadGroups AutoConfig{..} _
    | interval < 1    = error "autoReload: negative interval"
autoReloadGroups _ [] = error "autoReload: no paths to load"
autoReloadGroups auto@AutoConfig{..} paths = do
  cfg <- load' (Just auto) paths
  let files = map snd paths
      loop meta = do
        threadDelay (max interval 1 * 1000000)
        meta' <- getMeta files
        if meta' == meta
          then loop meta
          else (reload cfg `E.catch` onError) >> loop meta'
  tid <- forkIO $ loop =<< getMeta files
  return (cfg, tid)

-- | Save both a file's size and its last modification date, so we
-- have a better chance of detecting a modification on a crappy
-- filesystem with timestamp resolution of 1 second or worse.
type Meta = (FileOffset, EpochTime)

getMeta :: [Worth FilePath] -> IO [Maybe Meta]
getMeta paths = forM paths $ \path ->
   handle (\(_::SomeException) -> return Nothing) . fmap Just $ do
     st <- getFileStatus (worth path)
     return (fileSize st, modificationTime st)

{--
-- | Look up a name in the given 'ConfigCache'.  If a binding exists, and
-- the value can be 'convert'ed to the desired type, return the
-- converted value, otherwise 'Nothing'.
lookup :: Configured a => ConfigCache -> Name -> IO (Maybe a)
lookup ConfigCache{..} name =
    (convert . CB.lookup name) <$> readIORef cfgMap
--}

{--
-- | Look up a name in the given 'ConfigCache'.  If a binding exists, and
-- the value can be 'convert'ed to the desired type, return the
-- converted value, otherwise throw a 'KeyError'.
require :: Configured a => ConfigCache -> Name -> IO a
require cfg name = do
  val <- lookup cfg name
  case val of
    Just v -> return v
    _      -> throwIO . KeyError $ name
--}

{--
-- | Look up a name in the given 'ConfigCache'.  If a binding exists, and
-- the value can be converted to the desired type, return it,
-- otherwise return the default value.
lookupDefault :: Configured a =>
                 a
              -- ^ Default value to return if 'lookup' or 'convert'
              -- fails.
              -> ConfigCache -> Name -> IO a
lookupDefault def cfg name = fromMaybe def <$> lookup cfg name
--}

-- | Perform a simple dump of a 'ConfigCache' to @stdout@.
display :: ConfigCache -> IO ()
display ConfigCache{..} = print =<< readIORef cfgMap

-- | Read the current configuration stored in the cache.
readConfig :: ConfigCache -> IO Config
readConfig = (Config . ConfigPlan <$>) . readIORef . cfgMap

flatten :: [(Name, Worth Path)]
        -> H.HashMap (Worth Path) [Directive]
        -> IO (CB.CritBit Name Value)
flatten roots files = foldM doPath CB.empty roots
 where
  doPath m (pfx, f) = case H.lookup f files of
        Nothing -> return m
        Just ds -> foldM (directive pfx (worth f)) m ds

  directive pfx _ m (Bind name (String value)) = do
      v <- interpolate pfx value m
      return $! CB.insert (T.append pfx name) (String v) m
  directive pfx _ m (Bind name value) =
      return $! CB.insert (T.append pfx name) value m
  directive pfx f m (Group name xs) = foldM (directive pfx' f) m xs
      where pfx' = T.concat [pfx, name, "."]
  directive pfx f m (Import path) =
      let f' = relativize f path
      in  case H.lookup (Required (relativize f path)) files of
            Just ds -> foldM (directive pfx f') m ds
            _       -> return m
  directive _ _ m (DirectiveComment _) = return m

interpolate :: T.Text -> T.Text -> CB.CritBit Name Value -> IO T.Text
interpolate pfx s env
    | "$" `T.isInfixOf` s =
      case T.parseOnly interp s of
        Left err   -> throwIO $ ParseError "" err
        Right xs -> (L.toStrict . toLazyText . mconcat) <$> mapM interpret xs
    | otherwise = return s
 where
  lookupEnv name = msum $ map (flip CB.lookup env) fullnames
    where fullnames = map (T.intercalate ".")     -- ["a.b.c.x","a.b.x","a.x","x"]
                    . map (reverse . (name:)) -- [["a","b","c","x"],["a","b","x"],["a","x"],["x"]]
                    . tails                   -- [["c","b","a"],["b","a"],["a"],[]]
                    . reverse                 -- ["c","b","a"]
                    . filter (not . T.null)   -- ["a","b","c"]
                    . T.split (=='.')         -- ["a","b","c",""]
                    $ pfx                     -- "a.b.c."

  interpret (Literal x)   = return (fromText x)
  interpret (Interpolate name) =
      case lookupEnv name of
        Just (String x) -> return (fromText x)
        Just (Number r) ->
            case toBoundedInteger r :: Maybe Int64 of
              Just n  -> return (decimal n)
              Nothing -> return (realFloat (toRealFloat r :: Double))
        Just _          -> error "type error"
        _ -> do
          e <- try . getEnv . T.unpack $ name
          case e of
            Left (_::SomeException) ->
                throwIO . ParseError "" $ "no such variable " ++ show name
            Right x -> return (fromString x)

importsOf :: Path -> [Directive] -> [Worth Path]
importsOf path (Import ref : xs) = Required (relativize path ref)
                                 : importsOf path xs
importsOf path (Group _ ys : xs) = importsOf path ys ++ importsOf path xs
importsOf path (_ : xs)          = importsOf path xs
importsOf _    _                 = []

relativize :: Path -> Path -> Path
relativize parent child
  | T.head child == '/' = child
  | otherwise           = fst (T.breakOnEnd "/" parent) `T.append` child

loadOne :: Worth FilePath -> IO [Directive]
loadOne path = do
  es <- try . L.readFile . worth $ path
  case es of
    Left (err::SomeException) -> case path of
                                   Required _ -> throwIO err
                                   _          -> return []
    Right s -> do
            p <- evaluate (L.eitherResult $ L.parse topLevel s)
                 `E.catch` \(e::ParseError) ->
                 throwIO $ case e of
                             ParseError _ err -> ParseError (worth path) err
            case p of
              Left err -> throwIO (ParseError (worth path) err)
              Right ds -> return ds

-- | Subscribe for notifications.  The given action will be invoked
-- when any change occurs to a configuration property matching the
-- supplied pattern.
subscribe :: ConfigCache -> Pattern -> ChangeHandler -> IO ()
subscribe ConfigCache{..} pat act = do
  m' <- atomicModifyIORef cfgSubs $ \m ->
        let m' = H.insertWith (++) pat [act] m in (m', m')
  evaluate m' >> return ()

notifySubscribers :: ConfigCache -> CB.CritBit Name Value -> CB.CritBit Name Value
                  -> H.HashMap Pattern [ChangeHandler] -> IO ()
notifySubscribers ConfigCache{..} m m' subs = H.foldrWithKey go (return ()) subs
 where
  changedOrGone = CB.foldrWithKey check [] m
      where check n v nvs = case CB.lookup n m' of
                              Just v' | v /= v'   -> (n,Just v'):nvs
                                      | otherwise -> nvs
                              _                   -> (n,Nothing):nvs
  new = CB.foldrWithKey check [] m'
      where check n v nvs = case CB.lookup n m of
                              Nothing -> (n,v):nvs
                              _       -> nvs
  notify p n v a = a n v `E.catch` maybe report onError cfgAuto
    where report e = hPutStrLn stderr $
                     "*** a ChangeHandler threw an exception for " ++
                     show (p,n) ++ ": " ++ show e
  go p@(Exact n) acts next = (const next =<<) $ do
    let v' = CB.lookup n m'
    when (CB.lookup n m /= v') . mapM_ (notify p n v') $ acts
  go p@(Prefix n) acts next = (const next =<<) $ do
    let matching = filter (T.isPrefixOf n . fst)
    forM_ (matching new) $ \(n',v) -> mapM_ (notify p n' (Just v)) acts
    forM_ (matching changedOrGone) $ \(n',v) -> mapM_ (notify p n' v) acts

-- $format
--
-- A configuration file consists of a series of directives and
-- comments, encoded in UTF-8.  A comment begins with a \"@#@\"
-- character, and continues to the end of a line.
--
-- Files and directives are processed from first to last, top to
-- bottom.

-- $binding
--
-- A binding associates a name with a value.
--
-- > my_string = "hi mom! \u2603"
-- > your-int-33 = 33
-- > his_bool = on
-- > HerList = [1, "foo", off]
--
-- A name must begin with a Unicode letter, which is followed by zero
-- or more of a Unicode alphanumeric code point, hyphen \"@-@\", or
-- underscore \"@_@\".
--
-- Bindings are created or overwritten in the order in which they are
-- encountered.  It is legitimate for a name to be bound multiple
-- times, in which case the last value wins.
--
-- > a = 1
-- > a = true
-- > # value of a is now true, not 1

-- $types
--
-- The configuration file format supports the following data types:
--
-- * Booleans, represented as @on@ or @off@, @true@ or @false@.  These
--   are case sensitive, so do not try to use @True@ instead of
--   @true@!
--
-- * Decimal fractions,  expressed in scientific notation.
--
-- * Unicode strings, represented as text (possibly containing escape
--   sequences) surrounded by double quotes.
--
-- * Heterogeneous lists of values, represented as an opening square
--   bracket \"@[@\", followed by a series of comma-separated values,
--   ending with a closing square bracket \"@]@\".
--
-- The following escape sequences are recognised in a text string:
--
-- * @\\n@ - newline
--
-- * @\\r@ - carriage return
--
-- * @\\t@ - horizontal tab
--
-- * @\\\\@ - backslash
--
-- * @\\\"@ - double quote
--
-- * @\\u@/xxxx/ - Unicode character from the basic multilingual
--   plane, encoded as four hexadecimal digits
--
-- * @\\u@/xxxx/@\\u@/xxxx/ - Unicode character from an astral plane,
--   as two hexadecimal-encoded UTF-16 surrogates

-- $interp
--
-- Strings support interpolation, so that you can dynamically
-- construct a string based on data in your configuration or the OS
-- environment.
--
-- If a string value contains the special sequence \"@$(foo)@\" (for
-- any name @foo@), then the name @foo@ will be looked up in the
-- configuration data and its value substituted.  If that name cannot
-- be found, it will be looked up in the OS environment.
--
-- For security reasons, it is an error for a string interpolation
-- fragment to contain a name that cannot be found in either the
-- current configuration or the environment.
--
-- To represent a single literal \"@$@\" character in a string, double
-- it: \"@$$@\".

-- $group
--
-- It is possible to group a number of directives together under a
-- single prefix:
--
-- > my-group
-- > {
-- >   a = 1
-- >
-- >   # groups support nesting
-- >   nested {
-- >     b = "yay!"
-- >   }
-- > }
--
-- The name of a group is used as a prefix for the items in the
-- group. For instance, the value of \"@a@\" above can be retrieved
-- using 'lookup' by supplying the name \"@my-group.a@\", and \"@b@\"
-- will be named \"@my-group.nested.b@\".

-- $import
--
-- To import the contents of another configuration file, use the
-- @import@ directive.
--
-- > import "$(HOME)/etc/myapp.cfg"
--
-- Absolute paths are imported as is.  Relative paths are resolved with
-- respect to the file they are imported from.  It is an error for an
-- @import@ directive to name a file that does not exist, cannot be read,
-- or contains errors.
--
-- If an @import@ appears inside a group, the group's naming prefix
-- will be applied to all of the names imported from the given
-- configuration file.
--
-- Supposing we have a file named \"@foo.cfg@\":
--
-- > bar = 1
--
-- And another file that imports it into a group:
--
-- > hi {
-- >   import "foo.cfg"
-- > }
--
-- This will result in a value named \"@hi.bar@\".

-- $notify
--
-- To more efficiently support an application's need to dynamically
-- reconfigure, a subsystem may ask to be notified when a
-- configuration property is changed as a result of a reload, using
-- the 'subscribe' action.
