{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           Data.Text
import           Data.String.Conversions       (cs)
import           Data.Time.Clock               (NominalDiffTime)
import qualified Hasql                         as H
import qualified Hasql.Postgres                as P

import           Network.HTTP.Types.Header     (hAccept, hAuthorization)
import           Network.HTTP.Types.Status     (status415, status400)
import           Network.Wai                   (Application, Request (..), Response,
                                                requestHeaders)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest       (pickContentType)
import           PostgREST.Auth                (setRole, jwtClaims, claimsToSQL)
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (errResponse)
import           PostgREST.Types               (SqlFragment)

import           Prelude hiding(concat)

import qualified Data.Vector             as V
import qualified Hasql.Backend           as B
import qualified Data.Map.Lazy           as M

runWithClaims :: forall s. AppConfig -> NominalDiffTime ->
                 (Request -> H.Tx P.Postgres s Response) ->
                 Request -> H.Tx P.Postgres s Response
runWithClaims conf time app req = do
    _ <- H.unitEx $ stmt setAnon
    case split (== ' ') (cs auth) of
      ("Bearer" : tokenStr : _) ->
        case jwtClaims jwtSecret tokenStr time of
          Just claims ->
            if M.member "role" claims
            then do
              mapM_ H.unitEx $ stmt <$> claimsToSQL claims
              app req
            else invalidJWT
          _ -> invalidJWT
      _ -> app req
  where
    stmt c = B.Stmt c V.empty True
    hdrs = requestHeaders req
    jwtSecret = configJwtSecret conf
    auth = fromMaybe "" $ lookup hAuthorization hdrs
    anon = cs $ configAnonRole conf
    setAnon = setRole anon
    invalidJWT = return $ errResponse status400 "Invalid JWT"

setSchemaSearchPath :: [SqlFragment] -> (Request -> H.Tx P.Postgres s Response) ->
                       Request -> H.Tx P.Postgres s Response
setSchemaSearchPath schemas app req = do
  H.unitEx $ B.Stmt ("set local search_path to " <> intercalate "," schemas) V.empty True
  app req

unsupportedAccept :: Application -> Application
unsupportedAccept app req respond =
  case accept of
    Left _ -> respond $ errResponse status415 "Unsupported Accept header, try: application/json"
    Right _ -> app req respond
  where accept = pickContentType $ lookup hAccept $ requestHeaders req

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
  . unsupportedAccept
