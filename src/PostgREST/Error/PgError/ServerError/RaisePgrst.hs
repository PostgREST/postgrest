module PostgREST.Error.PgError.ServerError.RaisePgrst where

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
  Text ->
  Maybe Text ->
  Either ApiRequestError.ApiRequestError (PgRaiseErrMessage, PgRaiseErrDetails)
parseRaisePGRST m d = do
  msgJson <- maybeToRight (ApiRequestError.PGRSTParseError $ ApiRequestError.MsgParseError m) (JSON.decodeStrictText m)
  det <- maybeToRight (ApiRequestError.PGRSTParseError ApiRequestError.NoDetail) d
  detJson <- maybeToRight (ApiRequestError.PGRSTParseError $ ApiRequestError.DetParseError det) (JSON.decodeStrictText det)
  return (msgJson, detJson)
