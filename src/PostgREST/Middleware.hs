{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Text
import           Data.String.Conversions       (cs)
import qualified Hasql                         as H
import qualified Hasql.Postgres                as P

import           Network.HTTP.Types.Header     (hAccept, hAuthorization)
import           Network.HTTP.Types.Status     (status403, status415)
import           Network.Wai                   (Application, Request (..),
                                                Response, isSecure, requestHeaders,
                                                responseLBS)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.App                 (contentTypeForAccept)
import           PostgREST.Auth                (setRole, jwtClaims, claimsToSQL)
import           PostgREST.Config              (AppConfig (..), corsPolicy)

import           Prelude hiding(concat)

import qualified Data.Vector             as V
import qualified Hasql.Backend           as B
import qualified Data.Map.Lazy           as M

runWithClaims :: forall s. AppConfig ->
                 (Request -> H.Tx P.Postgres s Response) ->
                 Request -> H.Tx P.Postgres s Response
runWithClaims conf app req = do
    mapM_ H.unitEx $ stmt <$> env
    app req
 where
   stmt = (flip $ flip B.Stmt V.empty) True
   hdrs = requestHeaders req
   jwtSecret = (cs $ configJwtSecret conf) :: Text
   auth = fromMaybe "" $ lookup hAuthorization hdrs
   anon = cs $ configAnonRole conf
   claims =
     fromMaybe (M.fromList []) $
     case split (==' ') (cs auth) of
       ("Bearer" : jwt : _) -> jwtClaims jwtSecret jwt
       _ -> Nothing
   env = if M.member "role" claims
            then jwtEnv
            else setRole anon : jwtEnv
   jwtEnv = claimsToSQL claims

checkInsecure :: Application -> Application
checkInsecure app req respond =
  if not (isSecure req || isHerokuSecure)
    then respond $ responseLBS status403 [] "SSL is required"
    else app req respond
  where
    hdrs = requestHeaders req
    isHerokuSecure = lookup "x-forwarded-proto" hdrs == Just "https"

unsupportedAccept :: Application -> Application
unsupportedAccept app req respond = do
  let
    accept = lookup hAccept $ requestHeaders req
  if isNothing $ contentTypeForAccept accept
  then respond $ responseLBS status415 [] "Unsupported Accept header, try: application/json"
  else app req respond

defaultMiddle :: Bool -> Application -> Application
defaultMiddle secure = (if secure then checkInsecure else id)
  . gzip def . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
  . unsupportedAccept
