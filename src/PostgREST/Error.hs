{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PostgREST.Error (apiRequestErrResponse, pgErrResponse, errResponse, prettyUsageError) where

import           Protolude
import           Data.Aeson                ((.=))
import qualified Data.Aeson                as JSON
import qualified Hasql.Pool                as P
import qualified Hasql.Session             as H
import qualified Network.HTTP.Types.Status as HT
import           Network.Wai               (Response, responseLBS)
import           PostgREST.ApiRequest      (toHeader, ContentType(..), ApiRequestError(..))

apiRequestErrResponse :: ApiRequestError -> Response
apiRequestErrResponse err =
  case err of
    ErrorActionInappropriate -> errResponse HT.status405 "Bad Request"
    ErrorInvalidBody errorMessage -> errResponse HT.status400 $ toS errorMessage
    ErrorInvalidRange -> errResponse HT.status416 "HTTP Range error"

errResponse :: HT.Status -> Text -> Response
errResponse status message = jsonErrResponse status $ JSON.object ["message" .= message]

jsonErrResponse :: HT.Status -> JSON.Value -> Response
jsonErrResponse status message = responseLBS status [toHeader CTApplicationJSON] $ JSON.encode message

pgErrResponse :: Bool -> P.UsageError -> Response
pgErrResponse authed e =
  let status = httpStatus authed e
      jsonType = toHeader CTApplicationJSON
      wwwAuth = ("WWW-Authenticate", "Bearer")
      hdrs = if status == HT.status401
                then [jsonType, wwwAuth]
                else [jsonType] in
  responseLBS status hdrs (JSON.encode e)

prettyUsageError :: P.UsageError -> Text
prettyUsageError (P.ConnectionError e) =
  "Database connection error:\n" <> toS (fromMaybe "" e)
prettyUsageError e = show $ JSON.encode e

instance JSON.ToJSON P.UsageError where
  toJSON (P.ConnectionError e) = JSON.object [
    "code" .= ("" :: Text),
    "message" .= ("Connection error" :: Text),
    "details" .= (toS $ fromMaybe "" e :: Text)]
  toJSON (P.SessionError e) = JSON.toJSON e -- H.Error

instance JSON.ToJSON H.Error where
  toJSON (H.ResultError (H.ServerError c m d h)) = JSON.object [
    "code" .= (toS c::Text),
    "message" .= (toS m::Text),
    "details" .= (fmap toS d::Maybe Text),
    "hint" .= (fmap toS h::Maybe Text)]
  toJSON (H.ResultError (H.UnexpectedResult m)) = JSON.object [
    "message" .= (m::Text)]
  toJSON (H.ResultError (H.RowError i H.EndOfInput)) = JSON.object [
    "message" .= ("Row error: end of input"::Text),
    "details" .=
      ("Attempt to parse more columns than there are in the result"::Text),
    "details" .= (("Row number " <> show i)::Text)]
  toJSON (H.ResultError (H.RowError i H.UnexpectedNull)) = JSON.object [
    "message" .= ("Row error: unexpected null"::Text),
    "details" .= ("Attempt to parse a NULL as some value."::Text),
    "details" .= (("Row number " <> show i)::Text)]
  toJSON (H.ResultError (H.RowError i (H.ValueError d))) = JSON.object [
    "message" .= ("Row error: Wrong value parser used"::Text),
    "details" .= d,
    "details" .= (("Row number " <> show i)::Text)]
  toJSON (H.ResultError (H.UnexpectedAmountOfRows i)) = JSON.object [
    "message" .= ("Unexpected amount of rows"::Text),
    "details" .= i]
  toJSON (H.ClientError d) = JSON.object [
    "message" .= ("Database client error"::Text),
    "details" .= (fmap toS d::Maybe Text)]

httpStatus :: Bool -> P.UsageError -> HT.Status
httpStatus _ (P.ConnectionError _) = HT.status500
httpStatus authed (P.SessionError (H.ResultError (H.ServerError c _ _ _))) =
  case toS c of
    '0':'8':_ -> HT.status503 -- pg connection err
    '0':'9':_ -> HT.status500 -- triggered action exception
    '0':'L':_ -> HT.status403 -- invalid grantor
    '0':'P':_ -> HT.status403 -- invalid role specification
    "23503"   -> HT.status409 -- foreign_key_violation
    "23505"   -> HT.status409 -- unique_violation
    '2':'5':_ -> HT.status500 -- invalid tx state
    '2':'8':_ -> HT.status403 -- invalid auth specification
    '2':'D':_ -> HT.status500 -- invalid tx termination
    '3':'8':_ -> HT.status500 -- external routine exception
    '3':'9':_ -> HT.status500 -- external routine invocation
    '3':'B':_ -> HT.status500 -- savepoint exception
    '4':'0':_ -> HT.status500 -- tx rollback
    '5':'3':_ -> HT.status503 -- insufficient resources
    '5':'4':_ -> HT.status413 -- too complex
    '5':'5':_ -> HT.status500 -- obj not on prereq state
    '5':'7':_ -> HT.status500 -- operator intervention
    '5':'8':_ -> HT.status500 -- system error
    'F':'0':_ -> HT.status500 -- conf file error
    'H':'V':_ -> HT.status500 -- foreign data wrapper error
    "P0001"   -> HT.status400 -- default code for "raise"
    'P':'0':_ -> HT.status500 -- PL/pgSQL Error
    'X':'X':_ -> HT.status500 -- internal Error
    "42P01"   -> HT.status404 -- undefined table
    "42501"   -> if authed then HT.status403 else HT.status401 -- insufficient privilege
    _         -> HT.status400
httpStatus _ (P.SessionError (H.ResultError _)) = HT.status500
httpStatus _ (P.SessionError (H.ClientError _)) = HT.status503
