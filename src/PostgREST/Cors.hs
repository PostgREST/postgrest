{-|
Module      : PostgREST.Cors
Description : Wai Middleware to set cors policy.
-}
module PostgREST.Cors (middleware) where

import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Middleware.Cors as Wai

import Data.List (lookup)

import Protolude

middleware :: Wai.Middleware
middleware = Wai.cors corsPolicy

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Wai.Request -> Maybe Wai.CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin ->
    Just Wai.CorsResourcePolicy
    { Wai.corsOrigins = Just ([origin], True)
    , Wai.corsMethods = ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"]
    , Wai.corsRequestHeaders = "Authorization" : accHeaders
    , Wai.corsExposedHeaders = Just
      [ "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"]
    , Wai.corsMaxAge = Just $ 60*60*24
    , Wai.corsVaryOrigin = False
    , Wai.corsRequireOrigin = False
    , Wai.corsIgnoreFailures = True
    }
  Nothing -> Nothing
  where
    headers = Wai.requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . BS.strip) $ BS.split ',' hdrs
       -- Impossible case, Middleware.Cors will not evaluate this when
       -- the Access-Control-Request-Headers header is not set.
      Nothing   -> []
