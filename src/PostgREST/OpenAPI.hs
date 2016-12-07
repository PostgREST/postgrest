{-# LANGUAGE OverloadedStrings #-}

module PostgREST.OpenAPI (
  encodeOpenAPI
  , isMalformedProxyUri
  , pickProxy
  ) where

import           Control.Lens
import           Data.Aeson                  (decode, encode)
import           Data.HashMap.Strict.InsOrd  (InsOrdHashMap, fromList)
import           Data.Maybe                  (fromJust)
import           Data.String                 (IsString (..))
import           Data.Text                   (unpack, pack, concat, intercalate, init, tail, toLower)
import qualified Data.Set                    as Set
import           Network.URI                 (parseURI, isAbsoluteURI,
                                              URI (..), URIAuth (..))

import           Protolude hiding              (concat, (&), Proxy, get, intercalate)

import           Data.Swagger

import           PostgREST.ApiRequest        (ContentType(..), toMime)
import           PostgREST.Config            (prettyVersion)
import           PostgREST.QueryBuilder      (operators)
import           PostgREST.Types             (Table(..), Column(..), PgArg(..),
                                              Proxy(..), ProcDescription(..))

makeMimeList :: [ContentType] -> MimeList
makeMimeList cs = MimeList $ map (fromString . toS . toMime) cs

toSwaggerType :: Text -> SwaggerType t
toSwaggerType "text"      = SwaggerString
toSwaggerType "integer"   = SwaggerInteger
toSwaggerType "boolean"   = SwaggerBoolean
toSwaggerType "numeric"   = SwaggerNumber
toSwaggerType _           = SwaggerString

makeTableDef :: (Table, [Column], [Text]) -> (Text, Schema)
makeTableDef (t, cs, _) =
  let tn = tableName t in
      (tn, (mempty :: Schema)
        & type_ .~ SwaggerObject
        & properties .~ fromList (map makeProperty cs))

makeProperty :: Column -> (Text, Referenced Schema)
makeProperty c = (colName c, Inline u)
  where
    r = mempty :: Schema
    s = if null $ colEnum c
           then r
           else r & enum_ .~ decode (encode (colEnum c))
    t = s & type_ .~ toSwaggerType (colType c)
    u = t & format ?~ colType c

makeProcDef :: ProcDescription -> (Text, Schema)
makeProcDef pd = ("(rpc) " <> pdName pd, s)
  where
    s = (mempty :: Schema)
          & type_ .~ SwaggerObject
          & properties .~ fromList (map makeProcProperty (pdArgs pd))
          & required .~ map pgaName (filter pgaReq (pdArgs pd))

makeProcProperty :: PgArg -> (Text, Referenced Schema)
makeProcProperty (PgArg n t _) = (n, Inline s)
  where
    s = (mempty :: Schema)
          & type_ .~ toSwaggerType t
          & format ?~ t

makeOperatorPattern :: Text
makeOperatorPattern =
  intercalate "|"
  [ concat ["^", x, y, "[.]"] |
    x <- ["not[.]", ""],
    y <- map fst operators ]

makeRowFilter :: Column -> Param
makeRowFilter c =
  (mempty :: Param)
  & name .~ colName c
  & required ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamQuery
    & type_ .~ SwaggerString
    & format ?~ colType c
    & pattern ?~ makeOperatorPattern)

makeRowFilters :: [Column] -> [Param]
makeRowFilters = map makeRowFilter

makeOrderItems :: [Column] -> [Text]
makeOrderItems cs =
  [ concat [x, y, z] |
    x <- map colName cs,
    y <- [".asc", ".desc", ""],
    z <- [".nullsfirst", ".nulllast", ""]
  ]

makeRangeParams :: [Param]
makeRangeParams =
  [ (mempty :: Param)
    & name        .~ "Range"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamHeader
      & type_ .~ SwaggerString)
  , (mempty :: Param)
    & name        .~ "Range-Unit"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamHeader
      & type_ .~ SwaggerString
      & default_ .~ decode "\"items\"")
  , (mempty :: Param)
    & name        .~ "offset"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString)
  , (mempty :: Param)
    & name        .~ "limit"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString)
  ]

makePreferParam :: [Text] -> Param
makePreferParam ts =
  (mempty :: Param)
  & name        .~ "Prefer"
  & description ?~ "Preference"
  & required    ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamHeader
    & type_ .~ SwaggerString
    & enum_ .~ decode (encode ts))

makeSelectParam :: Param
makeSelectParam =
  (mempty :: Param)
    & name        .~ "select"
    & description ?~ "Filtering Columns"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString)

makeGetParams :: [Column] -> [Param]
makeGetParams [] =
  makeRangeParams ++
  [ makeSelectParam
  , makePreferParam ["count=none"]
  ]
makeGetParams cs =
  makeRangeParams ++
  [ makeSelectParam
  , (mempty :: Param)
    & name        .~ "order"
    & description ?~ "Ordering"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString
      & enum_ .~ decode (encode $ makeOrderItems cs))
  , makePreferParam ["count=none"]
  ]

makePostParams :: Text -> [Param]
makePostParams tn =
  [ makePreferParam ["return=representation",
                     "return=minimal", "return=none"]
  , (mempty :: Param)
    & name        .~ "body"
    & description ?~ tn
    & required    ?~ False
    & schema .~ ParamBody (Ref (Reference tn))
  ]

makeProcParam :: Text -> [Param]
makeProcParam refName =
  [ makePreferParam ["params=single-object"]
  , (mempty :: Param)
    & name     .~ "args"
    & required ?~ True
    & schema   .~ ParamBody (Ref (Reference refName))
  ]

makeDeleteParams :: [Param]
makeDeleteParams =
  [ makePreferParam ["return=representation", "return=minimal", "return=none"] ]

makePathItem :: (Table, [Column], [Text]) -> (FilePath, PathItem)
makePathItem (t, cs, _) = ("/" ++ unpack tn, p $ tableInsertable t)
  where
    tOp = (mempty :: Operation)
      & tags .~ Set.fromList [tn]
      & produces ?~ makeMimeList [CTApplicationJSON, CTTextCSV]
      & at 200 ?~ "OK"
    getOp = tOp
      & parameters .~ map Inline (makeGetParams cs ++ rs)
      & at 206 ?~ "Partial Content"
    postOp = tOp
      & consumes ?~ makeMimeList [CTApplicationJSON, CTTextCSV]
      & parameters .~ map Inline (makePostParams tn)
      & at 201 ?~ "Created"
    patchOp = tOp
      & consumes ?~ makeMimeList [CTApplicationJSON, CTTextCSV]
      & parameters .~ map Inline (makePostParams tn ++ rs)
      & at 204 ?~ "No Content"
    deletOp = tOp
      & parameters .~ map Inline (makeDeleteParams ++ rs)
    pr = (mempty :: PathItem) & get ?~ getOp
    pw = pr & post ?~ postOp & patch ?~ patchOp & delete ?~ deletOp
    p False = pr
    p True  = pw
    rs = makeRowFilters cs
    tn = tableName t

makeProcPathItem :: ProcDescription -> (FilePath, PathItem)
makeProcPathItem pd = ("/rpc/" ++ toS (pdName pd), pe)
  where
    postOp = (mempty :: Operation)
      & parameters .~ map Inline (makeProcParam $ "(rpc) " <> pdName pd)
      & tags .~ Set.fromList ["(rpc) " <> pdName pd]
      & produces ?~ makeMimeList [CTApplicationJSON]
      & at 200 ?~ "OK"
    pe = (mempty :: PathItem) & post ?~ postOp

makeRootPathItem :: (FilePath, PathItem)
makeRootPathItem = ("/", p)
  where
    getOp = (mempty :: Operation)
      & tags .~ Set.fromList ["/"]
      & produces ?~ makeMimeList [CTOpenAPI]
      & at 200 ?~ "OK"
    pr = (mempty :: PathItem) & get ?~ getOp
    p = pr

makePathItems :: [ProcDescription] -> [(Table, [Column], [Text])] -> InsOrdHashMap FilePath PathItem
makePathItems pds ti = fromList $ makeRootPathItem :
  map makePathItem ti ++ map makeProcPathItem pds

escapeHostName :: Text -> Text
escapeHostName "*"  = "0.0.0.0"
escapeHostName "*4" = "0.0.0.0"
escapeHostName "!4" = "0.0.0.0"
escapeHostName "*6" = "0.0.0.0"
escapeHostName "!6" = "0.0.0.0"
escapeHostName h    = h

postgrestSpec :: [ProcDescription] -> [(Table, [Column], [Text])] -> (Text, Text, Integer, Text) -> Swagger
postgrestSpec pds ti (s, h, p, b) = (mempty :: Swagger)
  & basePath ?~ unpack b
  & schemes ?~ [s']
  & info .~ ((mempty :: Info)
      & version .~ prettyVersion
      & title .~ "PostgREST API"
      & description ?~ "This is a dynamic API generated by PostgREST")
  & host .~ h'
  & definitions .~ fromList (map makeTableDef ti <> map makeProcDef pds)
  & paths .~ makePathItems pds ti
    where
      s' = if s == "http" then Http else Https
      h' = Just $ Host (unpack $ escapeHostName h) (Just (fromInteger p))

encodeOpenAPI :: [ProcDescription] -> [(Table, [Column], [Text])] -> (Text, Text, Integer, Text) -> LByteString
encodeOpenAPI pds ti uri = encode $ postgrestSpec pds ti uri

{-|
  Test whether a proxy uri is malformed or not.
  A valid proxy uri should be an absolute uri without query and user info,
  only http(s) schemes are valid, port number range is 1-65535.

  For example
  http://postgrest.com/openapi.json
  https://postgrest.com:8080/openapi.json
-}
isMalformedProxyUri :: Maybe Text -> Bool
isMalformedProxyUri Nothing =  False
isMalformedProxyUri (Just uri)
  | isAbsoluteURI (toS uri) = not $ isUriValid $ toURI uri
  | otherwise = True

toURI :: Text -> URI
toURI uri = fromJust $ parseURI (toS uri)

pickProxy :: Maybe Text -> Maybe Proxy
pickProxy proxy
  | isNothing proxy = Nothing
  -- should never happen
  -- since the request would have been rejected by the middleware if proxy uri
  -- is malformed
  | isMalformedProxyUri proxy = Nothing
  | otherwise = Just Proxy {
    proxyScheme = scheme
  , proxyHost = host'
  , proxyPort = port''
  , proxyPath = path'
  }
 where
   uri = toURI $ fromJust proxy
   scheme = init $ toLower $ pack $ uriScheme uri
   path URI {uriPath = ""} =  "/"
   path URI {uriPath = p} = p
   path' = pack $ path uri
   authority = fromJust $ uriAuthority uri
   host' = pack $ uriRegName authority
   port' = uriPort authority
   readPort = fromMaybe 80 . readMaybe
   port'' :: Integer
   port'' = case (port', scheme) of
             ("", "http") -> 80
             ("", "https") -> 443
             _ -> readPort $ unpack $ tail $ pack port'

isUriValid:: URI -> Bool
isUriValid = fAnd [isSchemeValid, isQueryValid, isAuthorityValid]

fAnd :: [a -> Bool] -> a -> Bool
fAnd fs x = all ($x) fs

isSchemeValid :: URI -> Bool
isSchemeValid URI {uriScheme = s}
  | toLower (pack s) == "https:" = True
  | toLower (pack s) == "http:" = True
  | otherwise = False

isQueryValid :: URI -> Bool
isQueryValid URI {uriQuery = ""} = True
isQueryValid _ = False

isAuthorityValid :: URI -> Bool
isAuthorityValid URI {uriAuthority = a}
  | isJust a = fAnd [isUserInfoValid, isHostValid, isPortValid] $ fromJust a
  | otherwise = False

isUserInfoValid :: URIAuth -> Bool
isUserInfoValid URIAuth {uriUserInfo = ""} = True
isUserInfoValid _ = False

isHostValid :: URIAuth -> Bool
isHostValid URIAuth {uriRegName = ""} = False
isHostValid _ = True

isPortValid :: URIAuth -> Bool
isPortValid URIAuth {uriPort = ""} = True
isPortValid URIAuth {uriPort = (':':p)} =
  case readMaybe p of
    Just i -> i > (0 :: Integer) && i < 65536
    Nothing -> False
isPortValid _ = False
