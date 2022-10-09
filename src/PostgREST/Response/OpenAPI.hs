{-|
Module      : PostgREST.OpenAPI
Description : Generates the OpenAPI output
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response.OpenAPI (encode) where

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet.InsOrd   as Set
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import Control.Arrow              ((&&&))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, fromList)
import Data.Maybe                 (fromJust)
import Data.String                (IsString (..))
import Network.URI                (URI (..), URIAuth (..))

import Control.Lens (at, (.~), (?~))

import Data.Swagger

import PostgREST.Config                   (AppConfig (..), Proxy (..),
                                           isMalformedProxyUri, toURI)
import PostgREST.SchemaCache              (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Proc         (ProcDescription (..),
                                           ProcParam (..))
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.SchemaCache.Table        (Column (..), Table (..),
                                           TablesMap)
import PostgREST.Version                  (docsVersion, prettyVersion)

import PostgREST.MediaType

import Protolude hiding (Proxy, get)

encode :: AppConfig -> SchemaCache -> TablesMap -> HM.HashMap k [ProcDescription] -> Maybe Text -> LBS.ByteString
encode conf sCache tables procs schemaDescription =
  JSON.encode $
    postgrestSpec
      (dbRelationships sCache)
      (concat $ HM.elems procs)
      (snd <$> HM.toList tables)
      (proxyUri conf)
      schemaDescription
      (configOpenApiSecurityActive conf)

makeMimeList :: [MediaType] -> MimeList
makeMimeList cs = MimeList $ fmap (fromString . BS.unpack . toMime) cs

toSwaggerType :: Text -> Maybe (SwaggerType t)
toSwaggerType "character varying" = Just SwaggerString
toSwaggerType "character"         = Just SwaggerString
toSwaggerType "text"              = Just SwaggerString
toSwaggerType "boolean"           = Just SwaggerBoolean
toSwaggerType "smallint"          = Just SwaggerInteger
toSwaggerType "integer"           = Just SwaggerInteger
toSwaggerType "bigint"            = Just SwaggerInteger
toSwaggerType "numeric"           = Just SwaggerNumber
toSwaggerType "real"              = Just SwaggerNumber
toSwaggerType "double precision"  = Just SwaggerNumber
toSwaggerType "ARRAY"             = Just SwaggerArray
toSwaggerType "json"              = Nothing
toSwaggerType "jsonb"             = Nothing
toSwaggerType _                   = Just SwaggerString

parseDefault :: Text -> Text -> Text
parseDefault colType colDefault =
  case toSwaggerType colType of
    Just SwaggerString -> wrapInQuotations $ case T.stripSuffix ("::" <> colType) colDefault of
      Just def -> T.dropAround (=='\'')  def
      Nothing  -> colDefault
    _ -> colDefault
  where
    wrapInQuotations text = "\"" <> text <> "\""

makeTableDef :: RelationshipsMap -> Table -> (Text, Schema)
makeTableDef rels t =
  let tn = tableName t in
      (tn, (mempty :: Schema)
        & description .~ tableDescription t
        & type_ ?~ SwaggerObject
        & properties .~ fromList (makeProperty t rels <$> tableColumns t)
        & required .~ fmap colName (filter (not . colNullable) $ tableColumns t))

makeProperty :: Table -> RelationshipsMap -> Column -> (Text, Referenced Schema)
makeProperty tbl rels col = (colName col, Inline s)
  where
    e = if null $ colEnum col then Nothing else JSON.decode $ JSON.encode $ colEnum col
    fk :: Maybe Text
    fk =
      let
        -- Finds the relationship that has a single column foreign key
        rel = find (\case
          Relationship{relCardinality=(M2O _ relColumns)} -> [colName col] == (fst <$> relColumns)
          _                                               -> False
          ) $ fromMaybe mempty $ HM.lookup (QualifiedIdentifier (tableSchema tbl) (tableName tbl), tableSchema tbl) rels
        fCol = (headMay . (\r -> snd <$> relColumns (relCardinality r)) =<< rel)
        fTbl = qiName . relForeignTable <$> rel
        fTblCol = (,) <$> fTbl <*> fCol
      in
        (\(a, b) -> T.intercalate "" ["This is a Foreign Key to `", a, ".", b, "`.<fk table='", a, "' column='", b, "'/>"]) <$> fTblCol
    pk :: Bool
    pk = colName col `elem` tablePKCols tbl
    n = catMaybes
      [ Just "Note:"
      , if pk then Just "This is a Primary Key.<pk/>" else Nothing
      , fk
      ]
    d =
      if length n > 1 then
        Just $ T.append (maybe "" (`T.append` "\n\n") $ colDescription col) (T.intercalate "\n" n)
      else
        colDescription col
    s =
      (mempty :: Schema)
        & default_ .~ (JSON.decode . toUtf8Lazy . parseDefault (colType col) =<< colDefault col)
        & description .~ d
        & enum_ .~ e
        & format ?~ colType col
        & maxLength .~ (fromIntegral <$> colMaxLen col)
        & type_ .~ toSwaggerType (colType col)

makeProcSchema :: ProcDescription -> Schema
makeProcSchema pd =
  (mempty :: Schema)
  & description .~ pdDescription pd
  & type_ ?~ SwaggerObject
  & properties .~ fromList (fmap makeProcProperty (pdParams pd))
  & required .~ fmap ppName (filter ppReq (pdParams pd))

makeProcProperty :: ProcParam -> (Text, Referenced Schema)
makeProcProperty (ProcParam n t _ _) = (n, Inline s)
  where
    s = (mempty :: Schema)
          & type_ .~ toSwaggerType t
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

makeParamDefs :: [Table] -> [(Text, Param)]
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
  <> concat [ makeObjectBody (tableName t) : makeRowFilters (tableName t) (tableColumns t)
            | t <- ti
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

makePathItem :: Table -> (FilePath, PathItem)
makePathItem t = ("/" ++ T.unpack tn, p $ tableInsertable t || tableUpdatable t || tableDeletable t)
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
    rs = [ T.intercalate "." ["rowFilter", tn, colName c ] | c <- tableColumns t ]
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
      & produces ?~ makeMimeList [MTApplicationJSON, MTSingularJSON]
      & at 200 ?~ "OK"
    pe = (mempty :: PathItem) & post ?~ postOp

makeRootPathItem :: (FilePath, PathItem)
makeRootPathItem = ("/", p)
  where
    getOp = (mempty :: Operation)
      & tags .~ Set.fromList ["Introspection"]
      & summary ?~ "OpenAPI description (this document)"
      & produces ?~ makeMimeList [MTOpenAPI, MTApplicationJSON]
      & at 200 ?~ "OK"
    pr = (mempty :: PathItem) & get ?~ getOp
    p = pr

makePathItems :: [ProcDescription] -> [Table] -> InsOrdHashMap FilePath PathItem
makePathItems pds ti = fromList $ makeRootPathItem :
  fmap makePathItem ti ++ fmap makeProcPathItem pds

makeSecurityDefinitions :: Text -> Bool -> SecurityDefinitions
makeSecurityDefinitions secName allow
  | allow = SecurityDefinitions (fromList [(secName, SecurityScheme secSchType secSchDescription)])
  | otherwise    = mempty
  where
    secSchType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)
    secSchDescription = Just "Add the token prepending \"Bearer \" (without quotes) to it"

escapeHostName :: Text -> Text
escapeHostName "*"  = "0.0.0.0"
escapeHostName "*4" = "0.0.0.0"
escapeHostName "!4" = "0.0.0.0"
escapeHostName "*6" = "0.0.0.0"
escapeHostName "!6" = "0.0.0.0"
escapeHostName h    = h

postgrestSpec :: RelationshipsMap -> [ProcDescription] -> [Table] -> (Text, Text, Integer, Text) -> Maybe Text -> Bool -> Swagger
postgrestSpec rels pds ti (s, h, p, b) sd allowSecurityDef = (mempty :: Swagger)
  & basePath ?~ T.unpack b
  & schemes ?~ [s']
  & info .~ ((mempty :: Info)
      & version .~ T.decodeUtf8 prettyVersion
      & title .~ "PostgREST API"
      & description ?~ d)
  & externalDocs ?~ ((mempty :: ExternalDocs)
    & description ?~ "PostgREST Documentation"
    & url .~ URL ("https://postgrest.org/en/" <> docsVersion <> "/api.html"))
  & host .~ h'
  & definitions .~ fromList (makeTableDef rels <$> ti)
  & parameters .~ fromList (makeParamDefs ti)
  & paths .~ makePathItems pds ti
  & produces .~ makeMimeList [MTApplicationJSON, MTSingularJSON, MTTextCSV]
  & consumes .~ makeMimeList [MTApplicationJSON, MTSingularJSON, MTTextCSV]
  & securityDefinitions .~ makeSecurityDefinitions securityDefName allowSecurityDef
  & security .~ [SecurityRequirement (fromList [(securityDefName, [])]) | allowSecurityDef]
    where
      s' = if s == "http" then Http else Https
      h' = Just $ Host (T.unpack $ escapeHostName h) (Just (fromInteger p))
      d = fromMaybe "This is a dynamic API generated by PostgREST" sd
      securityDefName = "JWT"

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
