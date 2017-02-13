{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
--module PostgREST.App where
module PostgREST.App (
  postgrest
) where

import           Control.Applicative
import qualified Data.ByteString.Char8     as BS
import           Data.Maybe
import           Data.IORef                (IORef, readIORef)
import           Data.Text                 (intercalate)
import           Data.Time.Clock.POSIX     (POSIXTime)

import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (renderSimpleQuery)
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.JWT                   (binarySecret)

import qualified Data.Vector               as V
import qualified Hasql.Transaction         as H

import qualified Data.HashMap.Strict       as M

import           PostgREST.ApiRequest   ( ApiRequest(..), ContentType(..)
                                        , Action(..), Target(..)
                                        , PreferRepresentation (..)
                                        , mutuallyAgreeable
                                        , toHeader
                                        , userApiRequest
                                        , toMime
                                        )
import           PostgREST.Auth            (jwtClaims, containsRole)
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.DbStructure
import           PostgREST.DbRequestBuilder( readRequest
                                           , mutateRequest
                                           , fieldNames
                                           )
import           PostgREST.Error           ( errResponse, pgErrResponse
                                           , apiRequestErrResponse
                                           , singularityError, binaryFieldError
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

postgrest :: AppConfig -> IORef DbStructure -> P.Pool -> IO POSIXTime ->
             Application
postgrest conf refDbStructure pool getTime =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle in

  middle $ \ req respond -> do
    time <- getTime
    body <- strictRequestBody req
    dbStructure <- readIORef refDbStructure

    response <- case userApiRequest (configSchema conf) req body of
      Left err -> return $ apiRequestErrResponse err
      Right apiRequest -> do
        let jwtSecret = binarySecret <$> configJwtSecret conf
            eClaims = jwtClaims jwtSecret (iJWT apiRequest) time
            authed = containsRole eClaims
            handleReq = runWithClaims conf eClaims (app dbStructure conf) apiRequest
            txMode = transactionMode $ iAction apiRequest
        response <- P.use pool $ HT.transaction HT.ReadCommitted txMode handleReq
        return $ either (pgErrResponse authed) identity response
    respond response

transactionMode :: Action -> H.Mode
transactionMode ActionRead = HT.Read
transactionMode ActionInfo = HT.Read
transactionMode _ = HT.Write

app :: DbStructure -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure conf apiRequest =
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

        (ActionCreate, TargetIdent (QualifiedIdentifier _ table), Just payload@(PayloadJSON rows)) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let isSingle = (==1) $ V.length rows
              if contentType == CTSingularJSON
                 && not isSingle
                 && iPreferRepresentation apiRequest == Full
                then return $ singularityError (toInteger $ V.length rows)
                else do
                  let pKeys = map pkName $ filter (filterPk schema table) allPrKeys -- would it be ok to move primary key detection in the query itself?
                      stm = createWriteStatement sq mq
                        (contentType == CTSingularJSON) isSingle
                        (contentType == CTTextCSV) (iPreferRepresentation apiRequest)
                        pKeys
                  row <- H.query payload stm
                  let (_, _, fs, body) = extractQueryResult row
                      headers = catMaybes [
                          if null fs
                            then Nothing
                            else Just (hLocation, "/" <> toS table <> renderLocationFields fs)
                        , if iPreferRepresentation apiRequest == Full
                            then Just $ toHeader contentType
                            else Nothing
                        , Just . contentRangeH 1 0 $
                            toInteger <$> if shouldCount then Just (V.length rows) else Nothing
                        ]

                  return . responseLBS status201 headers $
                    if iPreferRepresentation apiRequest == Full
                      then toS body else ""

        (ActionUpdate, TargetIdent _, Just payload) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) []
              row <- H.query payload stm
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
              let emptyPayload = PayloadJSON V.empty
                  stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False
                    (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) []
              row <- H.query emptyPayload stm
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

        (ActionInvoke, TargetProc qi, Just (PayloadJSON payload)) ->
          case readSqlParts of
            Left errorResponse -> return errorResponse
            Right (q, cq) -> do
              let p = V.head payload
                  singular = contentType == CTSingularJSON
                  paramsAsSingleObject = iPreferSingleObjectParameter apiRequest
              row <- H.query () (callProc qi p q cq topLevelRange shouldCount singular paramsAsSingleObject)
              let (tableTotal, queryTotal, body) =
                    fromMaybe (Just 0, 0, "[]") row
                  (status, contentRange) = rangeHeader queryTotal tableTotal
              if singular && queryTotal /= 1
                then do
                  HT.condemn
                  return $ singularityError (toInteger queryTotal)
                else return $ responseLBS status [jsonH, contentRange] (toS body)

        (ActionInspect, TargetRoot, Nothing) -> do
          let host = configHost conf
              port = toInteger $ configPort conf
              proxy = pickProxy $ toS <$> configProxyUri conf
              uri Nothing = ("http", host, port, "/")
              uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
              uri' = uri proxy
              encodeApi ti = encodeOpenAPI (map snd $ dbProcs dbStructure) ti uri'
          body <- encodeApi . toTableInfo <$> H.query schema accessibleTables
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
      filterCol _ _ _ =  False
      allPrKeys = dbPrimaryKeys dbStructure
      allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
      jsonH = toHeader CTApplicationJSON
      shouldCount = iPreferCount apiRequest
      schema = toS $ configSchema conf
      topLevelRange = fromMaybe allRange $ M.lookup "limit" $ iRange apiRequest
      rangeHeader queryTotal tableTotal =
        let lower = rangeOffset topLevelRange
            upper = lower + toInteger queryTotal - 1
            contentRange = contentRangeH lower upper (toInteger <$> tableTotal)
            status = rangeStatus lower upper (toInteger <$> tableTotal)
        in (status, contentRange)

      mapSnd f (a, b) = (a, f b)
      readReq = readRequest (configMaxRows conf) (dbRelations dbStructure) (map (mapSnd pdReturnType) $ dbProcs dbStructure) apiRequest
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
        ActionInvoke ->  [CTApplicationJSON, CTSingularJSON]
        ActionInspect -> [CTOpenAPI]
        ActionInfo ->    [CTTextCSV]
    serves sProduces cAccepts =
      case mutuallyAgreeable sProduces cAccepts of
        Nothing -> do
          let failed = intercalate ", " $ map (toS . toMime) cAccepts
          Left $ errResponse status415 $
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
