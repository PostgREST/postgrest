{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PostgREST.Error (PgError, pgErrResponse, errResponse) where


import           Data.Aeson                ((.=))
import qualified Data.Aeson                as JSON
import           Data.String.Conversions   (cs)
import           Data.String.Utils         (replace)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Hasql                     as H
import qualified Hasql.Postgres            as P
import           Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Status as HT
import           Network.Wai               (Response, responseLBS)

type PgError = H.SessionError P.Postgres

errResponse :: HT.Status -> Text -> Response
errResponse status message = responseLBS status [(hContentType, "application/json")] (cs $ T.concat ["{\"message\":\"",message,"\"}"])

pgErrResponse :: PgError -> Response
pgErrResponse e = responseLBS (httpStatus e)
  [(hContentType, "application/json")] (JSON.encode e)

instance JSON.ToJSON PgError where
  toJSON (H.TxError (P.ErroneousResult c m d h)) = JSON.object [
    "code" .= (cs c::T.Text),
    "message" .= (cs m::T.Text),
    "details" .= (fmap cs d::Maybe T.Text),
    "hint" .= (fmap cs h::Maybe T.Text)]
  toJSON (H.TxError (P.NoResult d)) = JSON.object [
    "message" .= ("No response from server"::T.Text),
    "details" .= (fmap cs d::Maybe T.Text)]
  toJSON (H.TxError (P.UnexpectedResult m)) = JSON.object ["message" .= m]
  toJSON (H.TxError P.NotInTransaction) = JSON.object [
    "message" .= ("Not in transaction"::T.Text)]
  toJSON (H.CxError (P.CantConnect d)) = JSON.object [
    "message" .= ("Can't connect to the database"::T.Text),
    "details" .= (fmap cs d::Maybe T.Text)]
  toJSON (H.CxError (P.UnsupportedVersion v)) = JSON.object [
    "message" .= ("Postgres version "++version++" is not supported") ]
      where version = replace "0" "." (show v)
  toJSON (H.ResultError m) = JSON.object ["message" .= m]

httpStatus :: PgError -> HT.Status
httpStatus (H.TxError (P.ErroneousResult codeBS _ _ _)) =
  let code = cs codeBS in
  case code of
    '0':'8':_ -> HT.status503 -- pg connection err
    '0':'9':_ -> HT.status500 -- triggered action exception
    '0':'L':_ -> HT.status403 -- invalid grantor
    '0':'P':_ -> HT.status403 -- invalid role specification
    "23503" -> HT.status409 -- foreign_key_violation
    "23505" -> HT.status409 -- unique_violation
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
    'P':'0':_ -> HT.status500 -- PL/pgSQL Error
    'X':'X':_ -> HT.status500 -- internal Error
    "42P01" -> HT.status404 -- undefined table
    "42501" -> HT.status404 -- insufficient privilege
    _ -> HT.status400
httpStatus (H.TxError (P.NoResult _)) = HT.status503
httpStatus _ = HT.status500
