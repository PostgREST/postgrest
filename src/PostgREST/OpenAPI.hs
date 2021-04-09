{-|
Module      : PostgREST.OpenAPI
Description : Generates the OpenAPI output
-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.OpenAPI (encode) where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.HashSet.InsOrd  as Set
import qualified Data.Text            as T

import Control.Arrow              ((&&&))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, fromList)
import Data.Maybe                 (fromJust)
import Data.String                (IsString (..))
import Network.URI                (URI (..), URIAuth (..))

import Control.Lens (at, (.~), (?~))

import Data.Swagger

import PostgREST.Config               (AppConfig (..), Proxy (..),
                                       docsVersion, prettyVersion, isMalformedProxyUri, toURI)
import PostgREST.DbStructure          (DbStructure (..), tableCols,
                                       tablePKCols)
import PostgREST.DbStructure.Proc     (PgArg (..),
                                       ProcDescription (..))
import PostgREST.DbStructure.Relation (PrimaryKey (..))
import PostgREST.DbStructure.Table    (Column (..), ForeignKey (..),
                                       Table (..))

import PostgREST.ContentType

import Protolude      hiding (Proxy, get, toS)
import Protolude.Conv (toS)

encode :: AppConfig -> DbStructure -> [Table] -> Maybe Text -> HashMap.HashMap k [ProcDescription] -> LBS.ByteString
encode conf dbStructure tables schemaDescription procs =
  JSON.encode $
    postgrestSpec
      (concat $ HashMap.elems procs)
      (openApiTableInfo dbStructure <$> tables)
      (proxyUri conf)
      schemaDescription
      (dbPrimaryKeys dbStructure)

makeMimeList :: [ContentType] -> MimeList
makeMimeList cs = MimeList $ fmap (fromString . toS . toMime) cs

toSwaggerType :: Text -> SwaggerType t
toSwaggerType "character varying" = SwaggerString
toSwaggerType "character"         = SwaggerString
toSwaggerType "text"              = SwaggerString
toSwaggerType "boolean"           = SwaggerBoolean
toSwaggerType "smallint"          = SwaggerInteger
toSwaggerType "integer"           = SwaggerInteger
toSwaggerType "bigint"            = SwaggerInteger
toSwaggerType "numeric"           = SwaggerNumber
toSwaggerType "real"              = SwaggerNumber
toSwaggerType "double precision"  = SwaggerNumber
toSwaggerType _                   = SwaggerString

makeTableDef :: [PrimaryKey] -> (Table, [Column], [Text]) -> (Text, Schema)
makeTableDef pks (t, cs, _) =
  let tn = tableName t in
      (tn, (mempty :: Schema)
        & description .~ tableDescription t
        & type_ ?~ SwaggerObject
        & properties .~ fromList (fmap (makeProperty pks) cs)
        & required .~ fmap colName (filter (not . colNullable) cs))

makeProperty :: [PrimaryKey] -> Column -> (Text, Referenced Schema)
makeProperty pks c = (colName c, Inline s)
  where
    e = if null $ colEnum c then Nothing else JSON.decode $ JSON.encode $ colEnum c
    fk ForeignKey{fkCol=Column{colTable=Table{tableName=a}, colName=b}} =
      T.intercalate "" ["This is a Foreign Key to `", a, ".", b, "`.<fk table='", a, "' column='", b, "'/>"]
    pk :: Bool
    pk = any (\p -> pkTable p == colTable c && pkName p == colName c) pks
    n = catMaybes
      [ Just "Note:"
      , if pk then Just "This is a Primary Key.<pk/>" else Nothing
      , fk <$> colFK c
      ]
    d =
      if length n > 1 then
        Just $ T.append (maybe "" (`T.append` "\n\n") $ colDescription c) (T.intercalate "\n" n)
      else
        colDescription c
    s =
      (mempty :: Schema)
        & default_ .~ (JSON.decode . toS =<< colDefault c)
        & description .~ d
        & enum_ .~ e
        & format ?~ colType c
        & maxLength .~ (fromIntegral <$> colMaxLen c)
        & type_ ?~ toSwaggerType (colType c)

makeProcSchema :: ProcDescription -> Schema
makeProcSchema pd =
  (mempty :: Schema)
  & description .~ pdDescription pd
  & type_ ?~ SwaggerObject
  & properties .~ fromList (fmap makeProcProperty (pdArgs pd))
  & required .~ fmap pgaName (filter pgaReq (pdArgs pd))

makeProcProperty :: PgArg -> (Text, Referenced Schema)
makeProcProperty (PgArg n t _ _) = (n, Inline s)
  where
    s = (mempty :: Schema)
          & type_ ?~ toSwaggerType t
          & format ?~ t

makePreferParam :: [Text] -> Param
makePreferParam ts =
  (mempty :: Param)
  & name        .~ "Prefer"
  & description ?~ "Preference"
  & required    ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamHeader
    & type_ ?~ SwaggerString
    & enum_ .~ JSON.decode (JSON.encode ts))

makeProcParam :: ProcDescription -> [Referenced Param]
makeProcParam pd =
  [ Inline $ (mempty :: Param)
    & name     .~ "args"
    & required ?~ True
    & schema   .~ ParamBody (Inline $ makeProcSchema pd)
  , Ref $ Reference "preferParams"
  ]

makeParamDefs :: [(Table, [Column], [Text])] -> [(Text, Param)]
makeParamDefs ti =
  [ ("preferParams", makePreferParam ["params=single-object"])
  , ("preferReturn", makePreferParam ["return=representation", "return=minimal", "return=none"])
  , ("preferCount", makePreferParam ["count=none"])
  , ("select", (mempty :: Param)
      & name        .~ "select"
      & description ?~ "Filtering Columns"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("on_conflict", (mempty :: Param)
      & name        .~ "on_conflict"
      & description ?~ "On Conflict"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("order", (mempty :: Param)
      & name        .~ "order"
      & description ?~ "Ordering"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("range", (mempty :: Param)
      & name        .~ "Range"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamHeader
        & type_ ?~ SwaggerString))
  , ("rangeUnit", (mempty :: Param)
      & name        .~ "Range-Unit"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamHeader
        & type_ ?~ SwaggerString
        & default_ .~ JSON.decode "\"items\""))
  , ("offset", (mempty :: Param)
      & name        .~ "offset"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("limit", (mempty :: Param)
      & name        .~ "limit"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  ]
  <> concat [ makeObjectBody (tableName t) : makeRowFilters (tableName t) cs
            | (t, cs, _) <- ti
            ]

makeObjectBody :: Text -> (Text, Param)
makeObjectBody tn =
  ("body." <> tn, (mempty :: Param)
     & name .~ tn
     & description ?~ tn
     & required ?~ False
     & schema .~ ParamBody (Ref (Reference tn)))

makeRowFilter :: Text -> Column -> (Text, Param)
makeRowFilter tn c =
  (T.intercalate "." ["rowFilter", tn, colName c], (mempty :: Param)
    & name .~ colName c
    & description .~ colDescription c
    & required ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ ?~ SwaggerString
      & format ?~ colType c))

makeRowFilters :: Text -> [Column] -> [(Text, Param)]
makeRowFilters tn = fmap (makeRowFilter tn)

makePathItem :: (Table, [Column], [Text]) -> (FilePath, PathItem)
makePathItem (t, cs, _) = ("/" ++ T.unpack tn, p $ tableInsertable t)
  where
    -- Use first line of table description as summary; rest as description (if present)
    -- We strip leading newlines from description so that users can include a blank line between summary and description
    (tSum, tDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> tableDescription t
    tOp = (mempty :: Operation)
      & tags .~ Set.fromList [tn]
      & summary .~ tSum
      & description .~ mfilter (/="") tDesc
    getOp = tOp
      & parameters .~ fmap ref (rs <> ["select", "order", "range", "rangeUnit", "offset", "limit", "preferCount"])
      & at 206 ?~ "Partial Content"
      & at 200 ?~ Inline ((mempty :: Response)
        & description .~ "OK"
        & schema ?~ Inline (mempty
          & type_ ?~ SwaggerArray
          & items ?~ SwaggerItemsObject (Ref $ Reference $ tableName t)
        )
      )
    postOp = tOp
      & parameters .~ fmap ref ["body." <> tn, "select", "preferReturn"]
      & at 201 ?~ "Created"
    patchOp = tOp
      & parameters .~ fmap ref (rs <> ["body." <> tn, "preferReturn"])
      & at 204 ?~ "No Content"
    deletOp = tOp
      & parameters .~ fmap ref (rs <> ["preferReturn"])
      & at 204 ?~ "No Content"
    pr = (mempty :: PathItem) & get ?~ getOp
    pw = pr & post ?~ postOp & patch ?~ patchOp & delete ?~ deletOp
    p False = pr
    p True  = pw
    tn = tableName t
    rs = [ T.intercalate "." ["rowFilter", tn, colName c ] | c <- cs ]
    ref = Ref . Reference

makeProcPathItem :: ProcDescription -> (FilePath, PathItem)
makeProcPathItem pd = ("/rpc/" ++ toS (pdName pd), pe)
  where
    -- Use first line of proc description as summary; rest as description (if present)
    -- We strip leading newlines from description so that users can include a blank line between summary and description
    (pSum, pDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> pdDescription pd
    postOp = (mempty :: Operation)
      & summary .~ pSum
      & description .~ mfilter (/="") pDesc
      & parameters .~ makeProcParam pd
      & tags .~ Set.fromList ["(rpc) " <> pdName pd]
      & produces ?~ makeMimeList [CTApplicationJSON, CTSingularJSON]
      & at 200 ?~ "OK"
    pe = (mempty :: PathItem) & post ?~ postOp

makeRootPathItem :: (FilePath, PathItem)
makeRootPathItem = ("/", p)
  where
    getOp = (mempty :: Operation)
      & tags .~ Set.fromList ["Introspection"]
      & summary ?~ "OpenAPI description (this document)"
      & produces ?~ makeMimeList [CTOpenAPI, CTApplicationJSON]
      & at 200 ?~ "OK"
    pr = (mempty :: PathItem) & get ?~ getOp
    p = pr

makePathItems :: [ProcDescription] -> [(Table, [Column], [Text])] -> InsOrdHashMap FilePath PathItem
makePathItems pds ti = fromList $ makeRootPathItem :
  fmap makePathItem ti ++ fmap makeProcPathItem pds

escapeHostName :: Text -> Text
escapeHostName "*"  = "0.0.0.0"
escapeHostName "*4" = "0.0.0.0"
escapeHostName "!4" = "0.0.0.0"
escapeHostName "*6" = "0.0.0.0"
escapeHostName "!6" = "0.0.0.0"
escapeHostName h    = h

postgrestSpec :: [ProcDescription] -> [(Table, [Column], [Text])] -> (Text, Text, Integer, Text) -> Maybe Text -> [PrimaryKey] -> Swagger
postgrestSpec pds ti (s, h, p, b) sd pks = (mempty :: Swagger)
  & basePath ?~ T.unpack b
  & schemes ?~ [s']
  & info .~ ((mempty :: Info)
      & version .~ prettyVersion
      & title .~ "PostgREST API"
      & description ?~ d)
  & externalDocs ?~ ((mempty :: ExternalDocs)
    & description ?~ "PostgREST Documentation"
    & url .~ URL ("https://postgrest.org/en/" <> docsVersion <> "/api.html"))
  & host .~ h'
  & definitions .~ fromList (makeTableDef pks <$> ti)
  & parameters .~ fromList (makeParamDefs ti)
  & paths .~ makePathItems pds ti
  & produces .~ makeMimeList [CTApplicationJSON, CTSingularJSON, CTTextCSV]
  & consumes .~ makeMimeList [CTApplicationJSON, CTSingularJSON, CTTextCSV]
    where
      s' = if s == "http" then Http else Https
      h' = Just $ Host (T.unpack $ escapeHostName h) (Just (fromInteger p))
      d = fromMaybe "This is a dynamic API generated by PostgREST" sd

pickProxy :: Maybe Text -> Maybe Proxy
pickProxy proxy
  | isNothing proxy = Nothing
  -- should never happen
  -- since the request would have been rejected by the middleware if proxy uri
  -- is malformed
  | isMalformedProxyUri $ fromMaybe mempty proxy = Nothing
  | otherwise = Just Proxy {
    proxyScheme = scheme
  , proxyHost = host'
  , proxyPort = port''
  , proxyPath = path'
  }
 where
   uri = toURI $ fromJust proxy
   scheme = T.init $ T.toLower $ T.pack $ uriScheme uri
   path URI {uriPath = ""} =  "/"
   path URI {uriPath = p}  = p
   path' = T.pack $ path uri
   authority = fromJust $ uriAuthority uri
   host' = T.pack $ uriRegName authority
   port' = uriPort authority
   readPort = fromMaybe 80 . readMaybe
   port'' :: Integer
   port'' = case (port', scheme) of
             ("", "http")  -> 80
             ("", "https") -> 443
             _             -> readPort $ T.unpack $ T.tail $ T.pack port'

proxyUri :: AppConfig -> (Text, Text, Integer, Text)
proxyUri AppConfig{..} =
  case pickProxy $ toS <$> configOpenApiServerProxyUri of
    Just Proxy{..} ->
      (proxyScheme, proxyHost, proxyPort, proxyPath)
    Nothing ->
      ("http", configServerHost, toInteger configServerPort, "/")

openApiTableInfo :: DbStructure -> Table -> (Table, [Column], [Text])
openApiTableInfo dbStructure table =
  ( table
  , tableCols dbStructure (tableSchema table) (tableName table)
  , tablePKCols dbStructure (tableSchema table) (tableName table)
  )
