module PostgREST.Error.ResultError.RaisePgrst where

import qualified Data.Aeson as JSON
import qualified PostgREST.Error.ApiRequestError as ApiRequestError
import Protolude

-- For parsing byteString to JSON Object, used for allowing full response control
data PgRaiseErrMessage = PgRaiseErrMessage
  { getCode :: Text,
    getMessage :: Text,
    getDetails :: Maybe Text,
    getHint :: Maybe Text
  }

instance JSON.FromJSON PgRaiseErrMessage where
  parseJSON (JSON.Object m) =
    PgRaiseErrMessage
      <$> m JSON..: "code"
      <*> m JSON..: "message"
      <*> m JSON..:? "details"
      <*> m JSON..:? "hint"
  parseJSON _ = mzero

data PgRaiseErrDetails = PgRaiseErrDetails
  { getStatus :: Int,
    getStatusText :: Maybe Text,
    getHeaders :: Map Text Text
  }

instance JSON.FromJSON PgRaiseErrDetails where
  parseJSON (JSON.Object d) =
    PgRaiseErrDetails
      <$> d JSON..: "status"
      <*> d JSON..:? "status_text"
      <*> d JSON..: "headers"
  parseJSON _ = mzero

parseRaisePGRST ::
  ByteString ->
  Maybe ByteString ->
  Either ApiRequestError.ApiRequestError (PgRaiseErrMessage, PgRaiseErrDetails)
parseRaisePGRST m d = do
  msgJson <- maybeToRight (ApiRequestError.PGRSTParseError $ ApiRequestError.MsgParseError m) (JSON.decodeStrict m)
  det <- maybeToRight (ApiRequestError.PGRSTParseError ApiRequestError.NoDetail) d
  detJson <- maybeToRight (ApiRequestError.PGRSTParseError $ ApiRequestError.DetParseError det) (JSON.decodeStrict det)
  return (msgJson, detJson)
