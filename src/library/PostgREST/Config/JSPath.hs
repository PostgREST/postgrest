{-|
Module      : PostgREST.Config.JSPath
Description : Parsing and evaluation logic of JSPath
-}
module PostgREST.Config.JSPath
  ( JSPath(..)
  , defaultRoleJSPathKey
  , dumpJSPath
  , pRoleClaimKey
  , evaluateJSPath
  ) where

import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.JSONPath           as JSP
import qualified Data.Aeson.JSONPath.Parser    as JSP
import qualified Data.Aeson.JSONPath.Types     as JSP
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Text.ParserCombinators.Parsec as P

import Data.Either.Combinators       (mapLeft)
import Data.Either.Extra             (fromRight')
import Text.ParserCombinators.Parsec ((<?>))

import Protolude


-- | full jspath, e.g. "$.property[0].attr.detail[?(@ == "role1")]"
newtype JSPath = JSPath JSP.Query

-- | Default value for "jwt-role-claim-key" config
defaultRoleJSPathKey :: JSPath
defaultRoleJSPathKey = fromRight' $ P.parse pJSPath "" "$.role"

-- | Dump JSPath
--   e.g. "$.property[0].attr.detail[?(@ == "role1")]"
dumpJSPath :: JSPath -> Text
dumpJSPath (JSPath query) = (escapeDollarChar . escapeDoubleQuotes) jsPathDump
    where
      jsPathDump = JSP.dumpQuery query
      escapeDoubleQuotes = T.replace "\"" "\\\""
      -- When dumping, $ must be escaped
      escapeDollarChar = T.replace "$" "$$"

-- |
-- Evaluate JSPath on a JSON
-- The result of JSON Path query is a Vector, we select the first
-- string element as the role.
evaluateJSPath :: Maybe JSON.Value -> JSPath -> Maybe JSON.Value
evaluateJSPath Nothing _                  = Nothing
evaluateJSPath (Just json) (JSPath query) = JSP.queryQQ query json V.!? 0

-- Used for the config value "role-claim-key"
pRoleClaimKey :: Text -> Either Text JSPath
pRoleClaimKey selStr =
  mapLeft show $ P.parse pJSPath ("failed to parse role-claim-key value (" <> toS selStr <> ")") (toS selStr)

-- | Parse RFC 9535 JSPath: $.roles[0]
pJSPath :: P.Parser JSPath
pJSPath = JSPath <$> JSP.pQuery <?> "pJSPath: JSPath root query"
