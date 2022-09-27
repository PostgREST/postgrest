{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response
  ( createResponse
  , deleteResponse
  , infoResponse
  , invokeResponse
  , openApiResponse
  , readResponse
  , singleUpsertResponse
  , updateResponse
  , addRetryHint
  , isServiceUnavailable
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Set                  as S
import           Data.Text.Read            (decimal)
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI    as HTTP
import qualified Network.Wai               as Wai

import qualified PostgREST.Error            as Error
import qualified PostgREST.MediaType        as MediaType
import qualified PostgREST.RangeQuery       as RangeQuery
import qualified PostgREST.Response.OpenAPI as OpenAPI

import PostgREST.Config                  (AppConfig (..))
import PostgREST.DbStructure             (DbStructure (..))
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (ProcDescription (..),
                                          ProcVolatility (..),
                                          ProcsMap)
import PostgREST.DbStructure.Table       (Table (..), TablesMap)
import PostgREST.GucHeader               (GucHeader,
                                          addHeadersIfNotIncluded,
                                          unwrapGucHeader)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.Request.ApiRequest      (ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))
import PostgREST.Request.Preferences     (PreferRepresentation (..),
                                          shouldCount,
                                          toAppliedHeader)
import PostgREST.Request.QueryParams     (QueryParams (..))

import qualified PostgREST.DbStructure.Proc as Proc
import qualified PostgREST.Request.Types    as ApiRequestTypes

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)


readResponse :: Bool -> QualifiedIdentifier -> ApiRequest -> Maybe Int64 -> ResultSet -> Wai.Response
readResponse headersOnly identifier ctxApiRequest@ApiRequest{..} total resultSet = case resultSet of
  RSStandard{..} -> do
    let
      (status, contentRange) = RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal total
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
          $ ApiRequestTypes.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show total)
        else LBS.fromStrict rsBody

    response status headers $ if headersOnly then mempty else rsOrErrBody

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

createResponse :: QualifiedIdentifier -> [FieldName] -> ApiRequest -> ResultSet -> Wai.Response
createResponse QualifiedIdentifier{..} pkCols ctxApiRequest@ApiRequest{..} resultSet = case resultSet of
  RSStandard{..} -> do
    let
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
              if shouldCount iPreferCount then Just rsQueryTotal else Nothing
          , if null pkCols && isNothing (qsOnConflict iQueryParams) then
              Nothing
            else
              toAppliedHeader <$> iPreferResolution
          ]

    if iPreferRepresentation == Full then
      response HTTP.status201 (headers ++ contentTypeHeaders ctxApiRequest) (LBS.fromStrict rsBody)
    else
      response HTTP.status201 headers mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

updateResponse :: ApiRequest -> ResultSet -> Wai.Response
updateResponse ctxApiRequest@ApiRequest{..} resultSet = case resultSet of
  RSStandard{..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders
      fullRepr = iPreferRepresentation == Full
      updateIsNoOp = S.null iColumns
      status
        | rsQueryTotal == 0 && not updateIsNoOp = HTTP.status404
        | fullRepr = HTTP.status200
        | otherwise = HTTP.status204
      contentRangeHeader =
        RangeQuery.contentRangeH 0 (rsQueryTotal - 1) $
          if shouldCount iPreferCount then Just rsQueryTotal else Nothing

    if fullRepr then
        response status (contentTypeHeaders ctxApiRequest ++ [contentRangeHeader]) (LBS.fromStrict rsBody)
      else
        response status [contentRangeHeader] mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

singleUpsertResponse :: ApiRequest -> ResultSet -> Wai.Response
singleUpsertResponse ctxApiRequest@ApiRequest{..} resultSet = case resultSet of
  RSStandard {..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders

    if iPreferRepresentation == Full then
      response HTTP.status200 (contentTypeHeaders ctxApiRequest) (LBS.fromStrict rsBody)
    else
      response HTTP.status204 [] mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

deleteResponse :: ApiRequest -> ResultSet -> Wai.Response
deleteResponse ctxApiRequest@ApiRequest{..} resultSet = case resultSet of
  RSStandard {..} -> do
    let
      response = gucResponse rsGucStatus rsGucHeaders
      contentRangeHeader =
        RangeQuery.contentRangeH 1 0 $
          if shouldCount iPreferCount then Just rsQueryTotal else Nothing

    if iPreferRepresentation == Full then
        response HTTP.status200
          (contentTypeHeaders ctxApiRequest ++ [contentRangeHeader])
          (LBS.fromStrict rsBody)
      else
        response HTTP.status204 [contentRangeHeader] mempty

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

infoResponse :: Target -> DbStructure -> Wai.Response
infoResponse target dbStructure =
  case target of
    TargetIdent identifier ->
      case HM.lookup identifier (dbTables dbStructure) of
        Just tbl -> respondInfo $ allowH tbl
        Nothing  -> Error.errorResponseFor $ Error.ApiRequestError ApiRequestTypes.NotFound
    TargetProc pd _
      | pdVolatility pd == Volatile -> respondInfo "OPTIONS,POST"
      | otherwise                   -> respondInfo "OPTIONS,GET,HEAD,POST"
    TargetDefaultSpec _             -> respondInfo "OPTIONS,GET,HEAD"
  where
    respondInfo allowHeader = Wai.responseLBS HTTP.status200 [allOrigins, (HTTP.hAllow, allowHeader)] mempty
    allOrigins = ("Access-Control-Allow-Origin", "*")
    allowH table =
      let hasPK = not . null $ tablePKCols table in
      BS.intercalate "," $
          ["OPTIONS,GET,HEAD"] ++
          ["POST" | tableInsertable table] ++
          ["PUT" | tableInsertable table && tableUpdatable table && hasPK] ++
          ["PATCH" | tableUpdatable table] ++
          ["DELETE" | tableDeletable table]

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

    if Proc.procReturnsVoid proc then
        response HTTP.status204 [contentRange] mempty
      else
        response status
          (contentTypeHeaders ctxApiRequest ++ [contentRange])
          (if invMethod == InvHead then mempty else rsOrErrBody)

  RSPlan plan ->
    Wai.responseLBS HTTP.status200 (contentTypeHeaders ctxApiRequest) $ LBS.fromStrict plan

openApiResponse :: Bool -> Maybe (TablesMap, ProcsMap, Maybe Text) -> AppConfig -> DbStructure -> Maybe Schema -> Wai.Response
openApiResponse headersOnly body conf dbStructure iProfile =
  Wai.responseLBS HTTP.status200
    (MediaType.toContentType MTOpenAPI : maybeToList (profileHeader iProfile))
    (maybe mempty (\(x, y, z) -> if headersOnly then mempty else OpenAPI.encode conf dbStructure x y z) body)

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
  MediaType.toContentType iAcceptMediaType : maybeToList (profileHeader iProfile)

profileHeader :: Maybe Schema -> Maybe HTTP.Header
profileHeader iProfile =
  (,) "Content-Profile" <$> (toS <$> iProfile)

addRetryHint :: Int -> Wai.Response -> Wai.Response
addRetryHint delay response = do
  let h = ("Retry-After", BS.pack $ show delay)
  Wai.mapResponseHeaders (\hs -> if isServiceUnavailable response then h:hs else hs) response

isServiceUnavailable :: Wai.Response -> Bool
isServiceUnavailable response = Wai.responseStatus response == HTTP.status503
