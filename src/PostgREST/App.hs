{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.App (
  postgrest
) where

import           Control.Applicative
import           Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import           Data.Maybe
import           Data.IORef                (IORef, readIORef)
import           Data.Text                 (intercalate)
import qualified Data.Set                  as S

import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (renderSimpleQuery)
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger (logStdout)

import qualified Hasql.Transaction         as H

import qualified Data.HashMap.Strict       as M

import           PostgREST.ApiRequest   ( ApiRequest(..), ContentType(..)
                                        , Action(..), Target(..)
                                        , PreferRepresentation (..)
                                        , mutuallyAgreeable
                                        , userApiRequest
                                        )
import           PostgREST.Auth            (jwtClaims, containsRole, parseJWK)
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.DbStructure
import           PostgREST.DbRequestBuilder( readRequest
                                           , mutateRequest
                                           , fieldNames
                                           )
import           PostgREST.Error           ( simpleError, pgError
                                           , apiRequestError
                                           , singularityError, binaryFieldError
                                           , connectionLostError, gucHeadersError
                                           )
import           PostgREST.RangeQuery      (allRange, rangeOffset)
import           PostgREST.Middleware
import           PostgREST.QueryBuilder ( callProc
                                        , requestToQuery
                                        , requestToCountQuery
                                        , createReadStatement
                                        , createWriteStatement
                                        , ResultsWithCount
                                        )
import           PostgREST.Types
import           PostgREST.OpenAPI

import           Data.Function (id)
import           Protolude              hiding (intercalate, Proxy)
import           Safe                   (headMay)

postgrest :: AppConfig -> IORef (Maybe DbStructure) -> P.Pool -> IO () -> Application
postgrest conf refDbStructure pool worker =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle
      jwtSecret = parseJWK <$> configJwtSecret conf in

  middle $ \ req respond -> do
    body <- strictRequestBody req
    maybeDbStructure <- readIORef refDbStructure
    case maybeDbStructure of
      Nothing -> respond connectionLostError
      Just dbStructure -> do
        response <- case userApiRequest (configSchema conf) req body of
          Left err -> return $ apiRequestError err
          Right apiRequest -> do
            eClaims <- jwtClaims jwtSecret (configJwtAudience conf) (toS $ iJWT apiRequest)

            let authed = containsRole eClaims
                proc = case (iTarget apiRequest, iPayload apiRequest, iPreferSingleObjectParameter apiRequest) of
                  (TargetProc qi, Just PayloadJSON{pjKeys=pKeys}, s) -> findProc qi pKeys s $ dbProcs dbStructure
                  _ -> Nothing
                handleReq = runWithClaims conf eClaims (app dbStructure proc conf) apiRequest
                txMode = transactionMode proc (iAction apiRequest)
            response <- P.use pool $ HT.transaction HT.ReadCommitted txMode handleReq
            return $ either (pgError authed) identity response
        when (responseStatus response == status503) worker
        respond response

findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> M.HashMap Text [ProcDescription] -> Maybe ProcDescription
findProc qi payloadKeys paramsAsSingleObject allProcs =
  let procs = M.lookup (qiName qi) allProcs in
 -- Handle overloaded functions case
  join $ (case length <$> procs of
    Just 1 -> headMay -- if it's not an overloaded function then immediatly get the ProcDescription
    _ -> find (\x ->
           if paramsAsSingleObject
             then length (pdArgs x) == 1 -- if the arg is not of json type let the db give the err
             else payloadKeys `S.isSubsetOf` S.fromList (pgaName <$> pdArgs x))
  ) <$> procs

transactionMode :: Maybe ProcDescription -> Action -> H.Mode
transactionMode proc action =
  case action of
    ActionRead -> HT.Read
    ActionInfo -> HT.Read
    ActionInspect -> HT.Read
    ActionInvoke{isReadOnly=False} ->
      let v = fromMaybe Volatile $ pdVolatility <$> proc in
      if v == Stable || v == Immutable
         then HT.Read
         else HT.Write
    ActionInvoke{isReadOnly=True} -> HT.Read
    _ -> HT.Write

app :: DbStructure -> Maybe ProcDescription -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure proc conf apiRequest =
  case responseContentTypeOrError (iAccepts apiRequest) (iAction apiRequest) of
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
              row <- H.query () stm
              let (tableTotal, queryTotal, _ , body) = row
                  (status, contentRange) = rangeHeader queryTotal tableTotal
                  canonical = iCanonicalQS apiRequest
              return $
                if contentType == CTSingularJSON && queryTotal /= 1
                  then singularityError (toInteger queryTotal)
                  else responseLBS status
                    [toHeader contentType, contentRange,
                      ("Content-Location",
                        "/" <> toS (qiName qi) <>
                          if BS.null canonical then "" else "?" <> toS canonical
                      )
                    ] (toS body)

        (ActionCreate, TargetIdent (QualifiedIdentifier _ table), Just (PayloadJSON payload pType _)) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let (isSingle, nRows) = case pType of
                                        PJArray len -> (len == 1, len)
                                        PJObject -> (True, 1)
              if contentType == CTSingularJSON
                 && not isSingle
                 && iPreferRepresentation apiRequest == Full
                then return $ singularityError (toInteger nRows)
                else do
                  let pKeys = map pkName $ filter (filterPk schema table) allPrKeys -- would it be ok to move primary key detection in the query itself?
                      stm = createWriteStatement sq mq
                        (contentType == CTSingularJSON) isSingle
                        (contentType == CTTextCSV) (iPreferRepresentation apiRequest)
                        pKeys
                  row <- H.query (toS payload) stm
                  let (_, _, fs, body) = extractQueryResult row
                      headers = catMaybes [
                          if null fs
                            then Nothing
                            else Just (hLocation, "/" <> toS table <> renderLocationFields fs)
                        , if iPreferRepresentation apiRequest == Full
                            then Just $ toHeader contentType
                            else Nothing
                        , Just . contentRangeH 1 0 $
                            toInteger <$> if shouldCount then Just nRows else Nothing
                        ]

                  return . responseLBS status201 headers $
                    if iPreferRepresentation apiRequest == Full
                      then toS body else ""

        (ActionUpdate, TargetIdent _, Just p@(PayloadJSON payload _ _)) ->
          case (mutateSqlParts, pjIsEmpty p, iPreferRepresentation apiRequest == Full) of
            (Left errorResponse, _, _) -> return errorResponse
            (_, True, True) -> return $ responseLBS status200 [contentRangeH 1 0 Nothing] "[]"
            (_, True, False) -> return $ responseLBS status204 [contentRangeH 1 0 Nothing] ""
            (Right (sq, mq), _, _) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) []
              row <- H.query (toS payload) stm
              let (_, queryTotal, _, body) = extractQueryResult row
              if contentType == CTSingularJSON
                 && queryTotal /= 1
                 && iPreferRepresentation apiRequest == Full
                then do
                  HT.condemn
                  return $ singularityError (toInteger queryTotal)
                else do
                  let r = contentRangeH 0 (toInteger $ queryTotal-1)
                            (toInteger <$> if shouldCount then Just queryTotal else Nothing)
                      s = if iPreferRepresentation apiRequest == Full
                            then status200
                            else status204
                  return $ if iPreferRepresentation apiRequest == Full
                    then responseLBS s [toHeader contentType, r] (toS body)
                    else responseLBS s [r] ""

        (ActionDelete, TargetIdent _, Nothing) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False
                    (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) []
              row <- H.query mempty stm
              let (_, queryTotal, _, body) = extractQueryResult row
                  r = contentRangeH 1 0 $
                        toInteger <$> if shouldCount then Just queryTotal else Nothing
              if contentType == CTSingularJSON
                 && queryTotal /= 1
                 && iPreferRepresentation apiRequest == Full
                then do
                  HT.condemn
                  return $ singularityError (toInteger queryTotal)
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

        (ActionInvoke _, TargetProc qi, Just (PayloadJSON payload pType pKeys)) ->
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
              let isObject = case pType of
                                PJObject  -> True
                                PJArray _ -> False
                  singular = contentType == CTSingularJSON
                  specifiedPgArgs = filter ((`S.member` pKeys) . pgaName) $ fromMaybe [] (pdArgs <$> proc)
              row <- H.query (toS payload) $
                callProc qi specifiedPgArgs returnsScalar q cq shouldCount
                         singular (iPreferSingleObjectParameter apiRequest)
                         (contentType == CTTextCSV)
                         (contentType == CTOctetStream) bField isObject
                         (pgVersion dbStructure)
              let (tableTotal, queryTotal, body, jsonHeaders) =
                    fromMaybe (Just 0, 0, "[]", "[]") row
                  (status, contentRange) = rangeHeader queryTotal tableTotal
                  decodedHeaders = first toS $ JSON.eitherDecode $ toS jsonHeaders :: Either Text [GucHeader]
              case decodedHeaders of
                Left _ -> return gucHeadersError
                Right hs ->
                  if singular && queryTotal /= 1
                    then do
                      HT.condemn
                      return $ singularityError (toInteger queryTotal)
                    else return $ responseLBS status ([toHeader contentType, contentRange] ++ toHeaders hs) (toS body)

        (ActionInspect, TargetRoot, Nothing) -> do
          let host = configHost conf
              port = toInteger $ configPort conf
              proxy = pickProxy $ toS <$> configProxyUri conf
              uri Nothing = ("http", host, port, "/")
              uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
              uri' = uri proxy
              encodeApi ti sd procs = encodeOpenAPI (concat $ M.elems procs) (toTableInfo ti) uri' sd (dbPrimaryKeys dbStructure)
          body <- encodeApi <$> H.query schema accessibleTables <*> H.query schema schemaDescription <*> H.query schema accessibleProcs
          return $ responseLBS status200 [toHeader CTOpenAPI] $ toS body

        _ -> return notFound

    where
      toTableInfo :: [Table] -> [(Table, [Column], [Text])]
      toTableInfo = map (\t ->
        let tSchema = tableSchema t
            tTable = tableName t
            cols = filter (filterCol tSchema tTable) $ dbColumns dbStructure
            pkeys = map pkName $ filter (filterPk tSchema tTable) allPrKeys
        in (t, cols, pkeys))
      notFound = responseLBS status404 [] ""
      filterPk sc table pk = sc == (tableSchema . pkTable) pk && table == (tableName . pkTable) pk
      filterCol :: Schema -> TableName -> Column -> Bool
      filterCol sc tb Column{colTable=Table{tableSchema=s, tableName=t}} = s==sc && t==tb
      allPrKeys = dbPrimaryKeys dbStructure
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
      mutateDbRequest = DbMutate <$> (mutateRequest apiRequest =<< fldNames)
      selectQuery = requestToQuery schema False <$> readDbRequest
      mutateQuery = requestToQuery schema False <$> mutateDbRequest
      countQuery = requestToCountQuery schema <$> readDbRequest
      readSqlParts = (,) <$> selectQuery <*> countQuery
      mutateSqlParts = (,) <$> selectQuery <*> mutateQuery

responseContentTypeOrError :: [ContentType] -> Action -> Either Response ContentType
responseContentTypeOrError accepts action = serves contentTypesForRequest accepts
  where
    contentTypesForRequest =
      case action of
        ActionRead ->    [CTApplicationJSON, CTSingularJSON, CTTextCSV, CTOctetStream]
        ActionCreate ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
        ActionUpdate ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
        ActionDelete ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
        ActionInvoke _ ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV, CTOctetStream]
        ActionInspect -> [CTOpenAPI, CTApplicationJSON]
        ActionInfo ->    [CTTextCSV]
    serves sProduces cAccepts =
      case mutuallyAgreeable sProduces cAccepts of
        Nothing -> do
          let failed = intercalate ", " $ map (toS . toMime) cAccepts
          Left $ simpleError status415 [] $
            "None of these Content-Types are available: " <> failed
        Just ct -> Right ct

binaryField :: ContentType -> [FieldName] -> Either Response (Maybe FieldName)
binaryField CTOctetStream fldNames =
  if length fldNames == 1 && fieldName /= Just "*"
    then Right fieldName
    else Left binaryFieldError
  where
    fieldName = headMay fldNames
binaryField _ _ = Right Nothing

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

contentRangeH :: Integer -> Integer -> Maybe Integer -> Header
contentRangeH lower upper total =
    ("Content-Range", headerValue)
    where
      headerValue   = rangeString <> "/" <> totalString
      rangeString
        | totalNotZero && fromInRange = show lower <> "-" <> show upper
        | otherwise = "*"
      totalString   = fromMaybe "*" (show <$> total)
      totalNotZero  = fromMaybe True ((/=) 0 <$> total)
      fromInRange   = lower <= upper

extractQueryResult :: Maybe ResultsWithCount -> ResultsWithCount
extractQueryResult = fromMaybe (Nothing, 0, [], "")
