-- |
-- Module      : PostgREST.ApiRequest.Payload
-- Description : Parser for PostgREST Request Body
--
-- This module is in charge of parsing the request body (payload)
--
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PostgREST.ApiRequest.Payload
  ( getPayload
  ) where

import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Csv              as CSV
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map.Strict       as M
import qualified Data.Scientific       as Sci
import qualified Data.Set              as S
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V

import Control.Arrow           ((***))
import Control.Monad           (fail)
import Data.Aeson              ((.:))
import Data.Aeson.Types        (emptyArray, emptyObject)
import Data.Either.Combinators (mapBoth, mapLeft)
import Network.HTTP.Types.URI  (parseSimpleQuery)

import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.ApiRequest.Types
import PostgREST.Error                   (ApiRequestError (..))
import PostgREST.MediaType               (MediaType (..))
import PostgREST.SchemaCache.Identifiers (FieldName)

import qualified PostgREST.MediaType as MediaType

import Protolude

getPayload :: RequestBody -> MediaType -> QueryParams -> Action -> Either ApiRequestError (Maybe Payload, S.Set FieldName)
getPayload reqBody contentMediaType QueryParams{qsColumns} action = do
  checkedPayload <- if shouldParsePayload then payload else Right Nothing
  let cols = case (checkedPayload, columns) of
        (Just ProcessedJSON{payKeys}, _)       -> payKeys
        (Just ProcessedUrlEncoded{payKeys}, _) -> payKeys
        (Just RawJSON{}, Just cls)             -> cls
        (Just PgrstPatchPay{}, Just cls)       -> cls
        _                                      -> S.empty
  return (checkedPayload, cols)
  where
    payload :: Either ApiRequestError (Maybe Payload)
    payload = mapBoth InvalidBody Just $ case (contentMediaType, isProc) of
      (MTApplicationJSON, _) ->
        if isJust columns
          then Right $ RawJSON reqBody
          else note "All object keys must match" . payloadAttributes reqBody
                 =<< if LBS.null reqBody && isProc
                       then Right emptyObject
                       else first BS.pack $
                          -- Drop parsing error message in favor of generic one (https://github.com/PostgREST/postgrest/issues/2344)
                          maybe (Left "Empty or invalid json") Right $ JSON.decode reqBody
      (MTTextCSV, _) -> do
        json <- csvToJson <$> first BS.pack (CSV.decodeByName reqBody)
        note "All lines must have same number of fields" $ payloadAttributes (JSON.encode json) json
      (MTUrlEncoded, True) ->
        Right $ ProcessedUrlEncoded params (S.fromList $ fst <$> params)
      (MTUrlEncoded, False) ->
        let paramsMap = HM.fromList $ (identity *** JSON.String) <$> params in
        Right $ ProcessedJSON (JSON.encode paramsMap) $ S.fromList (HM.keys paramsMap)
      (MTTextPlain, True) -> Right $ RawPay reqBody
      (MTTextXML, True) -> Right $ RawPay reqBody
      (MTOctetStream, True) -> Right $ RawPay reqBody
      (MTVndPgrstPatch, False) -> PgrstPatchPay <$> parsePgrstPatch reqBody
      (ct, _) -> Left $ "Content-Type not acceptable: " <> MediaType.toMime ct

    parsePgrstPatch :: LBS.ByteString -> Either ByteString [PgrstPatchOp]
    parsePgrstPatch = mapLeft BS.pack . JSON.eitherDecode

    shouldParsePayload = case action of
      ActDb (ActRelationMut _ MutationDelete) -> False
      ActDb (ActRelationMut _ _)              -> True
      ActDb (ActRoutine _  Inv)               -> True
      _                                       -> False

    columns = case action of
      ActDb (ActRelationMut _ MutationCreate) -> qsColumns
      ActDb (ActRelationMut _ MutationUpdate) -> qsColumns
      ActDb (ActRoutine     _ Inv)            -> qsColumns
      _                                       -> Nothing

    isProc = case action of
      ActDb (ActRoutine _ _) -> True
      _                      -> False
    params = (T.decodeUtf8 *** T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody)


type CsvData = V.Vector (M.Map Text LBS.ByteString)

{-|
  Converts CSV like
  a,b
  1,hi
  2,bye

  into a JSON array like
  [ {"a": "1", "b": "hi"}, {"a": 2, "b": "bye"} ]

  The reason for its odd signature is so that it can compose
  directly with CSV.decodeByName
-}
csvToJson :: (CSV.Header, CsvData) -> JSON.Value
csvToJson (_, vals) =
  JSON.Array $ V.map rowToJsonObj vals
 where
  rowToJsonObj = JSON.Object . KM.fromMapText .
    M.map (\str ->
        if str == "NULL"
          then JSON.Null
          else JSON.String . T.decodeUtf8 $ LBS.toStrict str
      )

payloadAttributes :: RequestBody -> JSON.Value -> Maybe Payload
payloadAttributes raw json =
  -- Test that Array contains only Objects having the same keys
  case json of
    JSON.Array arr ->
      case arr V.!? 0 of
        Just (JSON.Object o) ->
          let canonicalKeys = S.fromList $ K.toText <$> KM.keys o
              areKeysUniform = all (\case
                JSON.Object x -> S.fromList (K.toText <$> KM.keys x) == canonicalKeys
                _ -> False) arr in
          if areKeysUniform
            then Just $ ProcessedJSON raw canonicalKeys
            else Nothing
        Just _ -> Nothing
        Nothing -> Just emptyPJArray

    JSON.Object o -> Just $ ProcessedJSON raw (S.fromList $ K.toText <$> KM.keys o)

    -- truncate everything else to an empty array.
    _ -> Just emptyPJArray
  where
    emptyPJArray = ProcessedJSON (JSON.encode emptyArray) S.empty


instance JSON.FromJSON PgrstPatchOp where
  parseJSON (JSON.Object o) = do
    op    <- parseString o "op"
    path  <- parseString o "path"
    -- TODO: We need to decide what JSON "value"s are allowed in our
    --       our Pgrst Patch implementation.
    --       For now, we only have incr operator, so it's number only
    case op of
      "incr" -> Incr path <$> parseNumber o "value"
      _      -> fail $ "Unknown Pgrst Patch operation " ++ show op
    where
      parseString obj key = do
        val <- obj .: key
        case val of
          JSON.String txt -> pure txt
          _               -> fail $ "Expected JSON string for " ++ show key

      parseNumber obj key = do
        val <- obj .: key
        case val of
          JSON.Number num -> pure $ sciToInt num
          _               -> fail $ "Expected JSON number for " ++ show key
        where
          sciToInt :: Sci.Scientific -> Int
          sciToInt = fromMaybe 0 . Sci.toBoundedInteger

  parseJSON _ = mzero
