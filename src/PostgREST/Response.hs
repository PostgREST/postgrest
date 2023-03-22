{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response
  ( createResponse
  , deleteResponse
  , infoIdentResponse
  , infoProcResponse
  , infoRootResponse
  , invokeResponse
  , openApiResponse
  , readResponse
  , singleUpsertResponse
  , updateResponse
  , addRetryHint
  , isServiceUnavailable
  , optionalRollback
  , traceHeaderMiddleware
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashMap.Strict       as HM
import qualified Data.List                 as L
import           Data.Text.Read            (decimal)
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI    as HTTP
import qualified Network.Wai               as Wai

import qualified PostgREST.Error            as Error
import qualified PostgREST.MediaType        as MediaType
import qualified PostgREST.RangeQuery       as RangeQuery
import qualified PostgREST.Response.OpenAPI as OpenAPI

import PostgREST.ApiRequest              (ApiRequest (..),
                                          InvokeMethod (..))
import PostgREST.ApiRequest.Preferences  (PreferRepresentation (..),
                                          PreferTransaction (..),
                                          Preferences (..),
                                          shouldCount,
                                          toAppliedHeader)
import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (MutateReadPlan (..))
import PostgREST.Plan.MutatePlan         (MutatePlan (..))
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.Response.GucHeader      (GucHeader, unwrapGucHeader)
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.SchemaCache.Proc        (ProcDescription (..),
                                          ProcVolatility (..),
                                          ProcsMap)
import PostgREST.SchemaCache.Table       (Table (..), TablesMap)

import qualified PostgREST.ApiRequest.Types as ApiRequestTypes
import qualified PostgREST.SchemaCache.Proc as Proc

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)

readResponse :: Bool -> QualifiedIdentifier -> ApiRequest -> ResultSet -> Wai.Response
readResponse headersOnly identifier ctxApiRequest@ApiRequest{..} resultSet = case resultSet of
  RSStandard{..} -> do
    let
      (status, contentRange) = RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal rsTableTotal
      response = gucResponse rsGucStatus rsGucHeaders
      headers =
        [ contentRange
        , ( "Content-Location"
          , "/"
              <> toUtf8 (qiName identifier)
              <> if BS.null (qsCanonical iQueryParams) then mempty else "?" <> qsCanonical iQueryParams
          )
        ]
        ++ contentTypeHeaders ctxApiRequest
      rsOrErrBody = if status == HTTP.status416
        then Error.errorPayload $ Error.ApiRequestError $ ApiRequestTypes.InvalidRange
          $ ApiRequestTypes.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show rsTableTotal)
        else LBS.fromStrict rsBody

    response status headers $ if headersOnly then mempty else rsOrErrBody

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

createResponse :: QualifiedIdentifier -> MutateReadPlan -> ApiRequest -> ResultSet -> Wai.Response
createResponse QualifiedIdentifier{..} MutateReadPlan{mrMutatePlan} ctxApiRequest@ApiRequest{iPreferences=Preferences{..}, ..} resultSet = case resultSet of
  RSStandard{..} -> do
    let
      pkCols = case mrMutatePlan of { Insert{insPkCols} -> insPkCols; _ -> mempty;}
      response = gucResponse rsGucStatus rsGucHeaders
      headers =
        catMaybes
          [ if null rsLocation then
              Nothing
            else
              Just
                ( HTTP.hLocation
                , "/"
                    <> toUtf8 qiName
                    <> HTTP.renderSimpleQuery True rsLocation
                )
          , Just . RangeQuery.contentRangeH 1 0 $
              if shouldCount preferCount then Just rsQueryTotal else Nothing
          , if null pkCols && isNothing (qsOnConflict iQueryParams) then
              Nothing
            else
              toAppliedHeader <$> preferResolution
          , toAppliedHeader <$> preferUndefinedKeys
          ]

    if preferRepresentation == Full then
      response HTTP.status201 (headers ++ contentTypeHeaders ctxApiRequest) (LBS.fromStrict rsBody)
    else
      response HTTP.status201 headers mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

updateResponse :: ApiRequest -> ResultSet -> Wai.Response
updateResponse ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} resultSet = case resultSet of
  RSStandard{..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders
      contentRangeHeader =
        Just . RangeQuery.contentRangeH 0 (rsQueryTotal - 1) $
          if shouldCount preferCount then Just rsQueryTotal else Nothing
      headers = catMaybes [contentRangeHeader, toAppliedHeader <$> preferUndefinedKeys]

    if preferRepresentation == Full then
        response HTTP.status200
          (headers ++ contentTypeHeaders ctxApiRequest)
          (LBS.fromStrict rsBody)
      else
        response HTTP.status204 headers mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

singleUpsertResponse :: ApiRequest -> ResultSet -> Wai.Response
singleUpsertResponse ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} resultSet = case resultSet of
  RSStandard {..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders

    if preferRepresentation == Full then
      response HTTP.status200 (contentTypeHeaders ctxApiRequest) (LBS.fromStrict rsBody)
    else
      response HTTP.status204 [] mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

deleteResponse :: ApiRequest -> ResultSet -> Wai.Response
deleteResponse ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} resultSet = case resultSet of
  RSStandard {..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders
      contentRangeHeader =
        RangeQuery.contentRangeH 1 0 $
          if shouldCount preferCount then Just rsQueryTotal else Nothing
      headers = [contentRangeHeader]

    if preferRepresentation == Full then
        response HTTP.status200
          (headers ++ contentTypeHeaders ctxApiRequest)
          (LBS.fromStrict rsBody)
      else
        response HTTP.status204 headers mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

infoIdentResponse :: QualifiedIdentifier -> SchemaCache -> Wai.Response
infoIdentResponse identifier sCache =
  case HM.lookup identifier (dbTables sCache) of
    Just tbl -> respondInfo $ allowH tbl
    Nothing  -> Error.errorResponseFor $ Error.ApiRequestError ApiRequestTypes.NotFound
  where
    allowH table =
      let hasPK = not . null $ tablePKCols table in
      BS.intercalate "," $
          ["OPTIONS,GET,HEAD"] ++
          ["POST" | tableInsertable table] ++
          ["PUT" | tableInsertable table && tableUpdatable table && hasPK] ++
          ["PATCH" | tableUpdatable table] ++
          ["DELETE" | tableDeletable table]

infoProcResponse :: ProcDescription -> Wai.Response
infoProcResponse proc | pdVolatility proc == Volatile = respondInfo "OPTIONS,POST"
                      | otherwise                     = respondInfo "OPTIONS,GET,HEAD,POST"

infoRootResponse :: Wai.Response
infoRootResponse = respondInfo "OPTIONS,GET,HEAD"

respondInfo :: ByteString -> Wai.Response
respondInfo allowHeader =
  let allOrigins = ("Access-Control-Allow-Origin", "*") in
  Wai.responseLBS HTTP.status200 [allOrigins, (HTTP.hAllow, allowHeader)] mempty

invokeResponse :: InvokeMethod -> ProcDescription -> ApiRequest -> ResultSet -> Wai.Response
invokeResponse invMethod proc ctxApiRequest@ApiRequest{..} resultSet = case resultSet of
  RSStandard {..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders
      (status, contentRange) =
        RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal rsTableTotal
      rsOrErrBody = if status == HTTP.status416
        then Error.errorPayload $ Error.ApiRequestError $ ApiRequestTypes.InvalidRange
          $ ApiRequestTypes.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show rsTableTotal)
        else LBS.fromStrict rsBody
      headers = [contentRange]

    if Proc.procReturnsVoid proc then
        response HTTP.status204 headers mempty
      else
        response status
          (headers ++ contentTypeHeaders ctxApiRequest)
          (if invMethod == InvHead then mempty else rsOrErrBody)

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

openApiResponse :: Bool -> Maybe (TablesMap, ProcsMap, Maybe Text) -> AppConfig -> SchemaCache -> Schema -> Bool -> Wai.Response
openApiResponse headersOnly body conf sCache schema negotiatedByProfile =
  Wai.responseLBS HTTP.status200
    (MediaType.toContentType MTOpenAPI : maybeToList (profileHeader schema negotiatedByProfile))
    (maybe mempty (\(x, y, z) -> if headersOnly then mempty else OpenAPI.encode conf sCache x y z) body)

-- | Response with headers and status overridden from GUCs.
gucResponse
  :: Maybe Text
  -> Maybe BS.ByteString
  -> HTTP.Status
  -> [HTTP.Header]
  -> LBS.ByteString
  -> Wai.Response
gucResponse rsGucStatus rsGucHeaders status headers body =
  case (,) <$> decodeGucStatus rsGucStatus <*> decodeGucHeaders rsGucHeaders of
    Left err -> Error.errorResponseFor err
    Right (gucStatus, gucHeaders) ->
      Wai.responseLBS (fromMaybe status gucStatus) (addHeadersIfNotIncluded headers (map unwrapGucHeader gucHeaders)) body

decodeGucHeaders :: Maybe BS.ByteString -> Either Error.Error [GucHeader]
decodeGucHeaders =
  maybe (Right []) $ first (const Error.GucHeadersError) . JSON.eitherDecode . LBS.fromStrict

decodeGucStatus :: Maybe Text -> Either Error.Error (Maybe HTTP.Status)
decodeGucStatus =
  maybe (Right Nothing) $ first (const Error.GucStatusError) . fmap (Just . toEnum . fst) . decimal

contentTypeHeaders :: ApiRequest -> [HTTP.Header]
contentTypeHeaders ApiRequest{..} =
  MediaType.toContentType iAcceptMediaType : maybeToList (profileHeader iSchema iNegotiatedByProfile)

profileHeader :: Schema -> Bool -> Maybe HTTP.Header
profileHeader schema negotiatedByProfile =
  if negotiatedByProfile
    then Just $ (,) "Content-Profile" (toS schema)
  else
    Nothing

addRetryHint :: Int -> Wai.Response -> Wai.Response
addRetryHint delay response = do
  let h = ("Retry-After", BS.pack $ show delay)
  Wai.mapResponseHeaders (\hs -> if isServiceUnavailable response then h:hs else hs) response

isServiceUnavailable :: Wai.Response -> Bool
isServiceUnavailable response = Wai.responseStatus response == HTTP.status503

optionalRollback :: AppConfig -> ApiRequest -> ExceptT Error.Error IO Wai.Response -> ExceptT Error.Error IO Wai.Response
optionalRollback AppConfig{..} ApiRequest{iPreferences=Preferences{..}} resp = do
  newRes <- catchError resp $ return . Error.errorResponseFor
  return $ Wai.mapResponseHeaders preferenceApplied newRes
  where
    shouldCommit =
      configDbTxAllowOverride && preferTransaction == Just Commit
    shouldRollback =
      configDbTxAllowOverride && preferTransaction == Just Rollback
    preferenceApplied
      | shouldCommit =
          addHeadersIfNotIncluded
            [toAppliedHeader Commit]
      | shouldRollback =
          addHeadersIfNotIncluded
            [toAppliedHeader Rollback]
      | otherwise =
          identity

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [HTTP.Header] -> [HTTP.Header] -> [HTTP.Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders

traceHeaderMiddleware :: AppConfig -> Wai.Middleware
traceHeaderMiddleware AppConfig{configServerTraceHeader} app req respond =
  case configServerTraceHeader of
    Nothing -> app req respond
    Just hdr ->
      let hdrVal = L.lookup hdr $ Wai.requestHeaders req in
      app req (respond . Wai.mapResponseHeaders ([(hdr, fromMaybe mempty hdrVal)] ++))
