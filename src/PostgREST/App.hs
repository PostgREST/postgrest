{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.App (
  postgrest
) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as M
import qualified Data.Set                   as S
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as H
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import Data.Aeson                           as JSON
import Data.Function                        (id)
import Data.IORef                           (IORef, readIORef)
import Data.Time.Clock                      (UTCTime)
import Network.HTTP.Types.URI               (renderSimpleQuery)
import Network.Wai.Middleware.RequestLogger (logStdout)

import Control.Applicative
import Data.Maybe
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

import PostgREST.ApiRequest       (Action (..), ApiRequest (..),
                                   ContentType (..),
                                   PreferRepresentation (..),
                                   Target (..), mutuallyAgreeable,
                                   userApiRequest)
import PostgREST.Auth             (containsRole, jwtClaims,
                                   parseSecret)
import PostgREST.Config           (AppConfig (..))
import PostgREST.DbRequestBuilder (fieldNames, mutateRequest,
                                   readRequest)
import PostgREST.DbStructure
import PostgREST.Error            (PgError (..), SimpleError (..),
                                   errorResponseFor, singularityError)
import PostgREST.Middleware
import PostgREST.OpenAPI
import PostgREST.Parsers          (pRequestColumns)
import PostgREST.QueryBuilder     (ResultsWithCount, callProc,
                                   createReadStatement,
                                   createWriteStatement,
                                   requestToCountQuery,
                                   requestToQuery)
import PostgREST.RangeQuery       (allRange, rangeOffset)
import PostgREST.Types
import Protolude                  hiding (Proxy, intercalate)

postgrest :: AppConfig -> IORef (Maybe DbStructure) -> P.Pool -> IO UTCTime -> IO () -> Application
postgrest conf refDbStructure pool getTime worker =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle
      jwtSecret = parseSecret <$> configJwtSecret conf in

  middle $ \ req respond -> do
    time <- getTime
    body <- strictRequestBody req
    maybeDbStructure <- readIORef refDbStructure
    case maybeDbStructure of
      Nothing -> respond . errorResponseFor $ ConnectionLostError
      Just dbStructure -> do
        response <- do
          -- Need to parse ?columns early because findProc needs it to solve overloaded functions
          let apiReq = userApiRequest (configSchema conf) (configRootSpec conf) req body
              apiReqCols = (,) <$> apiReq <*> (pRequestColumns =<< iColumns <$> apiReq)
          case apiReqCols of
            Left err -> return . errorResponseFor $ err
            Right (apiRequest, maybeCols) -> do
              eClaims <- jwtClaims jwtSecret (configJwtAudience conf) (toS $ iJWT apiRequest) time (rightToMaybe $ configRoleClaimKey conf)
              let authed = containsRole eClaims
                  cols = case (iPayload apiRequest, maybeCols) of
                    (Just ProcessedJSON{pjKeys}, _) -> pjKeys
                    (Just RawJSON{}, Just cls)      -> cls
                    _                               -> S.empty
                  proc = case iTarget apiRequest of
                    TargetProc qi _ -> findProc qi cols (iPreferSingleObjectParameter apiRequest) $ dbProcs dbStructure
                    _ -> Nothing
                  handleReq = runWithClaims conf eClaims (app dbStructure proc cols conf) apiRequest
                  txMode = transactionMode proc (iAction apiRequest)
              response <- P.use pool $ HT.transaction HT.ReadCommitted txMode handleReq
              return $ either (errorResponseFor . PgError authed) identity response
        when (responseStatus response == status503) worker
        respond response

transactionMode :: Maybe ProcDescription -> Action -> HT.Mode
transactionMode proc action =
  case action of
    ActionRead -> HT.Read
    ActionInfo -> HT.Read
    ActionInspect -> HT.Read
    ActionInvoke{isReadOnly=False} ->
      let v = maybe Volatile pdVolatility proc in
      if v == Stable || v == Immutable
         then HT.Read
         else HT.Write
    ActionInvoke{isReadOnly=True} -> HT.Read
    _ -> HT.Write

app :: DbStructure -> Maybe ProcDescription -> S.Set FieldName -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure proc cols conf apiRequest =
  case responseContentTypeOrError (iAccepts apiRequest) (iAction apiRequest) (iTarget apiRequest) of
    Left errorResponse -> return errorResponse
    Right contentType ->
      case (iAction apiRequest, iTarget apiRequest, iPayload apiRequest) of

        (ActionRead, TargetIdent qi, Nothing) ->
          let partsField = (,) <$> readSqlParts
                <*> (binaryField contentType =<< fldNames) in
          case partsField of
            Left errorResponse -> return errorResponse
            Right ((q, cq), bField) -> do
              let stm = createReadStatement q cq (contentType == CTSingularJSON) shouldCount
                                            (contentType == CTTextCSV) bField
              row <- H.statement () stm
              let (tableTotal, queryTotal, _ , body) = row
                  (status, contentRange) = rangeHeader queryTotal tableTotal
                  canonical = iCanonicalQS apiRequest
              return $
                if contentType == CTSingularJSON && queryTotal /= 1
                  then errorResponseFor . singularityError $ queryTotal
                  else responseLBS status
                    [toHeader contentType, contentRange,
                      ("Content-Location",
                        "/" <> toS (qiName qi) <>
                          if BS.null canonical then "" else "?" <> toS canonical
                      )
                    ] (toS body)

        (ActionCreate, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let pkCols = tablePKCols dbStructure tSchema tName
                  stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) True
                    (contentType == CTTextCSV) (iPreferRepresentation apiRequest) pkCols
              row <- H.statement (toS $ pjRaw pJson) stm
              let (_, queryTotal, fs, body) = extractQueryResult row
                  headers = catMaybes [
                      if null fs
                        then Nothing
                        else Just (hLocation, "/" <> toS tName <> renderLocationFields fs)
                    , if iPreferRepresentation apiRequest == Full
                        then Just $ toHeader contentType
                        else Nothing
                    , Just $ contentRangeH 1 0 $
                        if shouldCount then Just queryTotal else Nothing
                    , if null pkCols
                        then Nothing
                        else (\x -> ("Preference-Applied", show x)) <$> iPreferResolution apiRequest
                    ]
              if contentType == CTSingularJSON
                 && queryTotal /= 1
                 && iPreferRepresentation apiRequest == Full
                then do
                  HT.condemn
                  return . errorResponseFor . singularityError $ queryTotal
              else
                return . responseLBS status201 headers $
                  if iPreferRepresentation apiRequest == Full
                    then toS body else ""

        (ActionUpdate, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) []
              row <- H.statement (toS $ pjRaw pJson) stm
              let (_, queryTotal, _, body) = extractQueryResult row

                  updateIsNoOp       = S.null cols
                  contentRangeHeader = contentRangeH 0 (queryTotal - 1) $
                                          if shouldCount then Just queryTotal else Nothing
                  minimalHeaders     = [contentRangeHeader]
                  fullHeaders        = toHeader contentType : minimalHeaders

                  status | queryTotal == 0 && not updateIsNoOp      = status404
                         | iPreferRepresentation apiRequest == Full = status200
                         | otherwise                                = status204

              case (contentType, iPreferRepresentation apiRequest) of
                (CTSingularJSON, Full)
                      | queryTotal == 1 -> return $ responseLBS status fullHeaders (toS body)
                      | otherwise       -> HT.condemn >> (return . errorResponseFor . singularityError) queryTotal

                (_, Full) ->
                  return $ responseLBS status fullHeaders (toS body)

                (_, _) ->
                  return $ responseLBS status minimalHeaders mempty


        (ActionSingleUpsert, TargetIdent (QualifiedIdentifier tSchema tName), Just ProcessedJSON{pjRaw, pjType, pjKeys}) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let isSingle = case pjType of
                               PJArray len -> len == 1
                               PJObject    -> True
                  colNames = colName <$> tableCols dbStructure tSchema tName
              if topLevelRange /= allRange
                then return . errorResponseFor $ PutRangeNotAllowedError
              else if not isSingle
                then return . errorResponseFor $ PutSingletonError
              else if S.fromList colNames /= pjKeys
                then return . errorResponseFor $ PutPayloadIncompleteError
              else do
                row <- H.statement (toS pjRaw) $
                       createWriteStatement sq mq (contentType == CTSingularJSON) False
                                            (contentType == CTTextCSV) (iPreferRepresentation apiRequest) []
                let (_, queryTotal, _, body) = extractQueryResult row
                -- Makes sure the querystring pk matches the payload pk
                -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted, PUT /items?id=eq.14 { "id" : 2, .. } is rejected
                -- If this condition is not satisfied then nothing is inserted, check the WHERE for INSERT in QueryBuilder.hs to see how it's done
                if queryTotal /= 1
                  then do
                    HT.condemn
                    return . errorResponseFor $ PutMatchingPkError
                  else
                    return $ if iPreferRepresentation apiRequest == Full
                      then responseLBS status200 [toHeader contentType] (toS body)
                      else responseLBS status204 [] ""

        (ActionDelete, TargetIdent (QualifiedIdentifier tSchema tName), Nothing) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False
                    (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) []
              row <- H.statement mempty stm
              let (_, queryTotal, _, body) = extractQueryResult row
                  r = contentRangeH 1 0 $
                        if shouldCount then Just queryTotal else Nothing
              if contentType == CTSingularJSON
                 && queryTotal /= 1
                 && iPreferRepresentation apiRequest == Full
                then do
                  HT.condemn
                  return . errorResponseFor . singularityError $ queryTotal
                else
                  return $ if iPreferRepresentation apiRequest == Full
                    then responseLBS status200 [toHeader contentType, r] (toS body)
                    else responseLBS status204 [r] ""

        (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable), Nothing) ->
          let mTable = find (\t -> tableName t == tTable && tableSchema t == tSchema) (dbTables dbStructure) in
          case mTable of
            Nothing -> return notFound
            Just table ->
              let acceptH = (hAllow, if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET") in
              return $ responseLBS status200 [allOrigins, acceptH] ""

        (ActionInvoke _, TargetProc qi _, Just pJson) ->
          let returnsScalar = case proc of
                Just ProcDescription{pdReturnType = (Single (Scalar _))} -> True
                _ -> False
              rpcBinaryField = if returnsScalar
                                 then Right Nothing
                                 else binaryField contentType =<< fldNames
              parts = (,) <$> readSqlParts <*> rpcBinaryField in
          case parts of
            Left errorResponse -> return errorResponse
            Right ((q, cq), bField) -> do
              let singular = contentType == CTSingularJSON
              row <- H.statement (toS $ pjRaw pJson) $
                callProc qi (specifiedProcArgs cols proc) returnsScalar q cq shouldCount
                         singular (iPreferSingleObjectParameter apiRequest)
                         (contentType == CTTextCSV)
                         (contentType `elem` rawContentTypes) bField
                         (pgVersion dbStructure)
              let (tableTotal, queryTotal, body, jsonHeaders) =
                    fromMaybe (Just 0, 0, "[]", "[]") row
                  (status, contentRange) = rangeHeader queryTotal tableTotal
                  decodedHeaders = first toS $ JSON.eitherDecode $ toS jsonHeaders :: Either Text [GucHeader]
              case decodedHeaders of
                Left _ -> return . errorResponseFor $ GucHeadersError
                Right hs ->
                  if singular && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else return $ responseLBS status ([toHeader contentType, contentRange] ++ toHeaders hs) (toS body)

        (ActionInspect, TargetDefaultSpec, Nothing) -> do
          let host = configHost conf
              port = toInteger $ configPort conf
              proxy = pickProxy $ toS <$> configProxyUri conf
              uri Nothing = ("http", host, port, "/")
              uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
              uri' = uri proxy
              toTableInfo :: [Table] -> [(Table, [Column], [Text])]
              toTableInfo = map (\t -> let (s, tn) = (tableSchema t, tableName t) in (t, tableCols dbStructure s tn, tablePKCols dbStructure s tn))
              encodeApi ti sd procs = encodeOpenAPI (concat $ M.elems procs) (toTableInfo ti) uri' sd $ dbPrimaryKeys dbStructure

          body <- encodeApi <$> H.statement schema accessibleTables <*> H.statement schema schemaDescription <*> H.statement schema accessibleProcs
          return $ responseLBS status200 [toHeader CTOpenAPI] $ toS body

        _ -> return notFound

    where
      notFound = responseLBS status404 [] ""
      allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
      shouldCount = iPreferCount apiRequest
      schema = toS $ configSchema conf
      topLevelRange = fromMaybe allRange $ M.lookup "limit" $ iRange apiRequest
      rangeHeader queryTotal tableTotal =
        let lower = rangeOffset topLevelRange
            upper = lower + toInteger queryTotal - 1
            contentRange = contentRangeH lower upper (toInteger <$> tableTotal)
            status = rangeStatus lower upper (toInteger <$> tableTotal)
        in (status, contentRange)

      readReq = readRequest (configMaxRows conf) (dbRelations dbStructure) proc apiRequest
      fldNames = fieldNames <$> readReq
      readDbRequest = DbRead <$> readReq
      selectQuery = requestToQuery schema False <$> readDbRequest
      countQuery = requestToCountQuery schema <$> readDbRequest
      readSqlParts = (,) <$> selectQuery <*> countQuery
      mutationDbRequest s t = mutateRequest apiRequest t cols (tablePKCols dbStructure s t) =<< fldNames
      mutateSqlParts s t =
        (,) <$> selectQuery
            <*> (requestToQuery schema False . DbMutate <$> mutationDbRequest s t)

responseContentTypeOrError :: [ContentType] -> Action -> Target -> Either Response ContentType
responseContentTypeOrError accepts action target = serves contentTypesForRequest accepts
  where
    contentTypesForRequest = case action of
      ActionRead         ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV] ++ rawContentTypes
      ActionCreate       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
      ActionUpdate       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
      ActionDelete       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
      ActionInvoke _     ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV] ++ rawContentTypes ++
                             [CTOpenAPI | tpIsRootSpec target]
      ActionInspect      ->  [CTOpenAPI, CTApplicationJSON]
      ActionInfo         ->  [CTTextCSV]
      ActionSingleUpsert ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
    serves sProduces cAccepts =
      case mutuallyAgreeable sProduces cAccepts of
        Nothing -> Left . errorResponseFor . ContentTypeError . map toMime $ cAccepts
        Just ct -> Right ct

{-
  | If raw(binary) output is requested, check that ContentType is one of the admitted rawContentTypes and that
  | `?select=...` contains only one field other than `*`
-}
binaryField :: ContentType -> [FieldName] -> Either Response (Maybe FieldName)
binaryField ct fldNames
  | ct `elem` rawContentTypes =
      let fieldName = headMay fldNames in
      if length fldNames == 1 && fieldName /= Just "*"
        then Right fieldName
        else Left . errorResponseFor $ BinaryFieldError ct
  | otherwise = Right Nothing

splitKeyValue :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitKeyValue kv = (k, BS.tail v)
  where (k, v) = BS.break (== '=') kv

renderLocationFields :: [BS.ByteString] -> BS.ByteString
renderLocationFields fields =
  renderSimpleQuery True $ map splitKeyValue fields

rangeStatus :: Integer -> Integer -> Maybe Integer -> Status
rangeStatus _ _ Nothing = status200
rangeStatus lower upper (Just total)
  | lower > total            = status416
  | (1 + upper - lower) < total = status206
  | otherwise               = status200

contentRangeH :: (Integral a, Show a) => a -> a -> Maybe a -> Header
contentRangeH lower upper total =
    ("Content-Range", headerValue)
    where
      headerValue   = rangeString <> "/" <> totalString
      rangeString
        | totalNotZero && fromInRange = show lower <> "-" <> show upper
        | otherwise = "*"
      totalString   = maybe "*" show total
      totalNotZero  = maybe True (0 /=) total
      fromInRange   = lower <= upper

extractQueryResult :: Maybe ResultsWithCount -> ResultsWithCount
extractQueryResult = fromMaybe (Nothing, 0, [], "")
