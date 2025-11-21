{- |
   Module      : PostgREST.Response
   Description : Generate HTTP Response
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response
  ( actionResponse
  , PgrstResponse(..)
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromJust)
import           Data.Text.Read            (decimal)
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI    as HTTP

import qualified PostgREST.Error            as Error
import qualified PostgREST.MediaType        as MediaType
import qualified PostgREST.RangeQuery       as RangeQuery
import qualified PostgREST.Response.OpenAPI as OpenAPI

import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferRepresentation (..),
                                          PreferResolution (..),
                                          Preferences (..),
                                          prefAppliedHeader,
                                          shouldCount)
import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.ApiRequest.Types        (InvokeMethod (..),
                                          Mutation (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.MainTx                  (DbResult (..),
                                          ResultSet (..))
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (CrudPlan (..),
                                          InfoPlan (..),
                                          InspectPlan (..))
import PostgREST.Plan.MutatePlan         (MutatePlan (..))
import PostgREST.Response.GucHeader      (GucHeader, unwrapGucHeader)
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.SchemaCache.Routine     (FuncVolatility (..),
                                          Routine (..))
import PostgREST.SchemaCache.Table       (Table (..))

import qualified PostgREST.SchemaCache.Routine as Routine

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)

data PgrstResponse = PgrstResponse {
  pgrstStatus  :: HTTP.Status
, pgrstHeaders :: [HTTP.Header]
, pgrstBody    :: LBS.ByteString
}

actionResponse :: DbResult -> ApiRequest -> (Text, Text) -> AppConfig -> SchemaCache -> Schema -> Bool -> Either Error.Error PgrstResponse

actionResponse (DbCrudResult WrappedReadPlan{pMedia, wrHdrsOnly=headersOnly, crudQi=identifier} RSStandard{..}) ctxApiRequest@ApiRequest{iPreferences=Preferences{..},..} _ _ _ _ _ = do
  let
    (status, contentRange) = RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal rsTableTotal
    cLHeader = if headersOnly then mempty else [contentLengthHeaderStrict rsBody]
    prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing Nothing preferCount preferTransaction Nothing preferHandling preferTimezone Nothing []
    headers =
      [ contentRange
      , ( "Content-Location"
        , "/"
            <> toUtf8 (qiName identifier)
            <> if BS.null (qsCanonical iQueryParams) then mempty else "?" <> qsCanonical iQueryParams
        )
      ]
      ++ cLHeader
      ++ contentTypeHeaders pMedia ctxApiRequest
      ++ prefHeader
    bod | status == HTTP.status416 = Error.errorPayload $ Error.ApiRequestError $ Error.InvalidRange $
                                     Error.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show rsTableTotal)
        | headersOnly              = mempty
        | otherwise                = LBS.fromStrict rsBody

  (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers

  Right $ PgrstResponse ovStatus ovHeaders bod

actionResponse (DbCrudResult MutateReadPlan{mrMutation=MutationCreate, mrMutatePlan, pMedia, crudQi=QualifiedIdentifier{..}} RSStandard{..}) ctxApiRequest@ApiRequest{iPreferences=Preferences{..}, ..} _ _ _ _ _ = do
  let
    pkCols = case mrMutatePlan of { Insert{insPkCols} -> insPkCols; _ -> mempty;}
    prefHeader = prefAppliedHeader $
      Preferences (if null pkCols && isNothing (qsOnConflict iQueryParams) then Nothing else preferResolution)
      preferRepresentation preferCount preferTransaction preferMissing preferHandling preferTimezone Nothing []
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
        , Just $ contentLengthHeaderStrict rsBody
        , prefHeader ]

    isInsertIfGTZero i =
        if i <= 0 && preferResolution == Just MergeDuplicates then
          HTTP.status200
        else
          HTTP.status201
    status = maybe HTTP.status200 isInsertIfGTZero rsInserted
    (headers', bod) = case preferRepresentation of
      Just Full -> (headers ++ contentTypeHeaders pMedia ctxApiRequest, LBS.fromStrict rsBody)
      Just None -> (headers, mempty)
      Just HeadersOnly -> (headers, mempty)
      Nothing -> (headers, mempty)

  (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers'

  Right $ PgrstResponse ovStatus ovHeaders bod

actionResponse (DbCrudResult MutateReadPlan{mrMutation=MutationUpdate, pMedia} RSStandard{..}) ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} _ _ _ _ _ = do
  let
    contentRangeHeader =
      Just . RangeQuery.contentRangeH 0 (rsQueryTotal - 1) $
        if shouldCount preferCount then Just rsQueryTotal else Nothing
    prefHeader = prefAppliedHeader $ Preferences Nothing preferRepresentation preferCount preferTransaction preferMissing preferHandling preferTimezone preferMaxAffected []
    headers = catMaybes [contentRangeHeader, prefHeader]

  let (status, headers', body) =
        case preferRepresentation of
          Just Full -> (HTTP.status200, headers ++ [contentLengthHeaderStrict rsBody] ++ contentTypeHeaders pMedia ctxApiRequest, LBS.fromStrict rsBody)
          Just None -> (HTTP.status204, headers, mempty)
          _         -> (HTTP.status204, headers, mempty)

  (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers'

  Right $ PgrstResponse ovStatus ovHeaders body

actionResponse (DbCrudResult MutateReadPlan{mrMutation=MutationSingleUpsert, pMedia} RSStandard{..}) ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} _ _ _ _ _ = do
  let
    prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing preferRepresentation preferCount preferTransaction Nothing preferHandling preferTimezone Nothing []
    cLHeader = [contentLengthHeaderStrict rsBody]
    cTHeader = contentTypeHeaders pMedia ctxApiRequest

  let isInsertIfGTZero i = if i > 0 then HTTP.status201 else HTTP.status200
      upsertStatus       = isInsertIfGTZero $ fromJust rsInserted
      (status, headers, body) =
        case preferRepresentation of
          Just Full -> (upsertStatus, cLHeader ++ cTHeader ++ prefHeader, LBS.fromStrict rsBody)
          Just None -> (HTTP.status204,  prefHeader, mempty)
          _ -> (HTTP.status204, prefHeader, mempty)
  (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers

  Right $ PgrstResponse ovStatus ovHeaders body

actionResponse (DbCrudResult MutateReadPlan{mrMutation=MutationDelete, pMedia} RSStandard{..}) ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} _ _ _ _ _ = do
  let
    contentRangeHeader = RangeQuery.contentRangeH 1 0 $ if shouldCount preferCount then Just rsQueryTotal else Nothing
    prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing preferRepresentation preferCount preferTransaction Nothing preferHandling preferTimezone preferMaxAffected []
    headers = contentRangeHeader : prefHeader
    (status, headers', body) =
        case preferRepresentation of
            Just Full -> (HTTP.status200, headers ++ [contentLengthHeaderStrict rsBody] ++ contentTypeHeaders pMedia ctxApiRequest, LBS.fromStrict rsBody)
            Just None -> (HTTP.status204, headers, mempty)
            _ -> (HTTP.status204, headers, mempty)

  (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers'

  Right $ PgrstResponse ovStatus ovHeaders body

actionResponse (DbCrudResult CallReadPlan{pMedia, crInvMthd=invMethod, crProc=proc} RSStandard {..}) ctxApiRequest@ApiRequest{iPreferences=Preferences{..},..} _ _ _ _ _ = do
  let
    (status, contentRange) =
      RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal rsTableTotal
    rsOrErrBody = if status == HTTP.status416
      then Error.errorPayload $ Error.ApiRequestError $ Error.InvalidRange
        $ Error.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show rsTableTotal)
      else LBS.fromStrict rsBody
    isHeadMethod = invMethod == InvRead True
    prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing Nothing preferCount preferTransaction Nothing preferHandling preferTimezone preferMaxAffected []
    cLHeader = if isHeadMethod then mempty else [contentLengthHeaderLazy rsOrErrBody]
    headers = contentRange : prefHeader
    (status', headers', body) =
        if Routine.funcReturnsVoid proc then
            (HTTP.status204, headers, mempty)
          else
            (status,
              headers ++ cLHeader ++ contentTypeHeaders pMedia ctxApiRequest,
              if isHeadMethod then mempty else rsOrErrBody)

  (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status' headers'

  Right $ PgrstResponse ovStatus ovHeaders body

actionResponse (DbPlanResult media plan) ctxApiRequest _ _ _ _ _ =
  Right $ PgrstResponse HTTP.status200 (contentLengthHeaderStrict plan : contentTypeHeaders media ctxApiRequest) $ LBS.fromStrict plan

actionResponse (MaybeDbResult InspectPlan{ipHdrsOnly=headersOnly} body) _ versions conf sCache schema negotiatedByProfile =
  let
    rsBody = maybe mempty (\(x, y, z) -> if headersOnly then mempty else OpenAPI.encode versions conf sCache x y z) body
    cLHeader = if headersOnly then mempty else [contentLengthHeaderLazy rsBody]
  in
  Right $ PgrstResponse HTTP.status200 (MediaType.toContentType MTOpenAPI : cLHeader ++ maybeToList (profileHeader schema negotiatedByProfile)) rsBody

actionResponse (NoDbResult (RelInfoPlan qi)) _ _ _ SchemaCache{dbTables} _ _ =
  case HM.lookup qi dbTables of
    Just tbl -> respondInfo $ allowH tbl
    Nothing  -> Left $ Error.ApiRequestError Error.InvalidResourcePath
  where
    allowH table =
      let hasPK = not . null $ tablePKCols table in
      BS.intercalate "," $
          ["OPTIONS,GET,HEAD"] ++
          ["POST" | tableInsertable table] ++
          ["PUT" | tableInsertable table && tableUpdatable table && hasPK] ++
          ["PATCH" | tableUpdatable table] ++
          ["DELETE" | tableDeletable table]

actionResponse (NoDbResult (RoutineInfoPlan proc)) _ _ _ _ _ _
  | pdVolatility proc == Volatile = respondInfo "OPTIONS,POST"
  | otherwise                     = respondInfo "OPTIONS,GET,HEAD,POST"

actionResponse (NoDbResult SchemaInfoPlan) _ _ _ _ _ _ = respondInfo "OPTIONS,GET,HEAD"

respondInfo :: ByteString -> Either Error.Error PgrstResponse
respondInfo allowHeader =
  let allOrigins = ("Access-Control-Allow-Origin", "*") in
  Right $ PgrstResponse HTTP.status200 [contentLengthHeaderStrict mempty, allOrigins, (HTTP.hAllow, allowHeader)] mempty

-- Status and headers can be overridden as per https://postgrest.org/en/stable/references/transactions.html#response-headers
overrideStatusHeaders :: Maybe Text -> Maybe BS.ByteString -> HTTP.Status -> [HTTP.Header]-> Either Error.Error (HTTP.Status, [HTTP.Header])
overrideStatusHeaders rsGucStatus rsGucHeaders pgrstStatus pgrstHeaders = do
  gucStatus <- decodeGucStatus rsGucStatus
  gucHeaders <- decodeGucHeaders rsGucHeaders
  Right (fromMaybe pgrstStatus gucStatus, addHeadersIfNotIncluded pgrstHeaders $ map unwrapGucHeader gucHeaders)

decodeGucHeaders :: Maybe BS.ByteString -> Either Error.Error [GucHeader]
decodeGucHeaders =
  maybe (Right []) $ first (const . Error.ApiRequestError $ Error.GucHeadersError) . JSON.eitherDecode . LBS.fromStrict

decodeGucStatus :: Maybe Text -> Either Error.Error (Maybe HTTP.Status)
decodeGucStatus =
  maybe (Right Nothing) $ first (const . Error.ApiRequestError $ Error.GucStatusError) . fmap (Just . toEnum . fst) . decimal

contentLengthHeader :: Show b => (a -> b) -> a -> HTTP.Header
contentLengthHeader lenFn body = ("Content-Length", show (lenFn body))

contentLengthHeaderStrict :: BS.ByteString -> HTTP.Header
contentLengthHeaderStrict = contentLengthHeader BS.length

contentLengthHeaderLazy :: LBS.ByteString -> HTTP.Header
contentLengthHeaderLazy = contentLengthHeader LBS.length

contentTypeHeaders :: MediaType -> ApiRequest -> [HTTP.Header]
contentTypeHeaders mediaType ApiRequest{..} =
  MediaType.toContentType mediaType : maybeToList (profileHeader iSchema iNegotiatedByProfile)

profileHeader :: Schema -> Bool -> Maybe HTTP.Header
profileHeader schema negotiatedByProfile =
  if negotiatedByProfile
    then Just $ (,) "Content-Profile" (toS schema)
  else
    Nothing

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [HTTP.Header] -> [HTTP.Header] -> [HTTP.Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders
