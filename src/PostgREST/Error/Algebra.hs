module PostgREST.Error.Algebra where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Lazy      as LBS
import qualified Network.HTTP.Types        as HTTP
import qualified PostgREST.MediaType       as MediaType

import Network.Wai (Response, responseLBS)
import Protolude

class (ErrorBody a, JSON.ToJSON a) => PgrstError a where
  status   :: a -> HTTP.Status
  headers  :: a -> [HTTP.Header]

  errorPayload :: a -> LByteString
  errorPayload = JSON.encode

  errorResponseFor :: a -> Response
  errorResponseFor err =
    let
      baseHeader = MediaType.toContentType MediaType.MTApplicationJSON
      cLHeader body = (,) "Content-Length" (show $ LBS.length body) :: HTTP.Header
    in
    responseLBS (status err) (baseHeader : cLHeader (errorPayload err) : headers err) $ errorPayload err

class ErrorBody a where
  code    :: a -> Text
  message :: a -> Text
  details :: a -> Maybe JSON.Value
  hint    :: a -> Maybe JSON.Value

toJsonPgrstError :: Text -> Text -> Maybe JSON.Value -> Maybe JSON.Value -> JSON.Value
toJsonPgrstError code' message' details' hint' = JSON.object [
    "code"     JSON..= code'
  , "message"  JSON..= message'
  , "details"  JSON..= details'
  , "hint"     JSON..= hint'
  ]
