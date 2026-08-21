{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-|
Module      : PostgREST.Config.DeprecatedJSPath
-}
module PostgREST.Config.DeprecatedJSPath
  ( DeprecatedJSPath
  , dumpDeprecatedJSPath
  , pDeprecatedRoleClaimKey
  , evaluateDeprecatedJSPath
  ) where

import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Key                as K
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Text.ParserCombinators.Parsec as P

import Data.Either.Combinators       (mapLeft)
import Text.ParserCombinators.Parsec ((<?>))
import Text.Read                     (read)

import Protolude

-- Deprecated JSPath
-- =================
-- This is the old custom JSPath DSL syntax which is deprecated and will be
-- removed in the next major (v18) release.
-- TODO: Delete this module before the next major released.

type DeprecatedJSPath = [JSPathExp]

-- NOTE: We only accept one JSPFilter expr (at the end of input)
-- | jspath expression
data JSPathExp
  = JSPKey Text         -- .property or ."property-dash"
  | JSPIdx Int          -- [0]
  | JSPFilter FilterExp -- [?(@ == "match")]

data FilterExp
  = EqualsCond Text
  | NotEqualsCond Text
  | StartsWithCond Text
  | EndsWithCond Text
  | ContainsCond Text

dumpDeprecatedJSPath :: DeprecatedJSPath -> Text
dumpDeprecatedJSPath djsp = T.intercalate mempty (fmap dumpDepJSPathExp djsp)
  where
    dumpDepJSPathExp :: JSPathExp -> Text
    -- TODO: this needs to be quoted properly for special chars
    dumpDepJSPathExp (JSPKey k) = "." <> show k
    dumpDepJSPathExp (JSPIdx i) = "[" <> show i <> "]"
    dumpDepJSPathExp (JSPFilter cond) = "[?(@" <> expr <> ")]"
      where
        expr =
          case cond of
            EqualsCond text     -> " == " <> show text
            NotEqualsCond text  -> " != " <> show text
            StartsWithCond text -> " ^== " <> show text
            EndsWithCond text   -> " ==^ " <> show text
            ContainsCond text   -> " *== " <> show text

-- | Evaluate JSPath on a JSON
evaluateDeprecatedJSPath :: Maybe JSON.Value -> DeprecatedJSPath -> Maybe JSON.Value
evaluateDeprecatedJSPath x                      []                = x
evaluateDeprecatedJSPath (Just (JSON.Object o)) (JSPKey key:rest) = evaluateDeprecatedJSPath (KM.lookup (K.fromText key) o) rest
evaluateDeprecatedJSPath (Just (JSON.Array ar)) (JSPIdx idx:rest) = evaluateDeprecatedJSPath (ar V.!? idx) rest
evaluateDeprecatedJSPath (Just (JSON.Array ar)) [JSPFilter jspFilter] = case jspFilter of
    EqualsCond txt     -> findFirstMatch (==) txt ar
    NotEqualsCond txt  -> findFirstMatch (/=) txt ar
    StartsWithCond txt -> findFirstMatch T.isPrefixOf txt ar
    EndsWithCond txt   -> findFirstMatch T.isSuffixOf txt ar
    ContainsCond txt   -> findFirstMatch T.isInfixOf txt ar
  where
    findFirstMatch matchWith pattern = find (\case
      JSON.String txt -> pattern `matchWith` txt
      _               -> False)
evaluateDeprecatedJSPath _                      _                 = Nothing

-- Used for the config value "role-claim-key"
pDeprecatedRoleClaimKey :: Text -> Either Text DeprecatedJSPath
pDeprecatedRoleClaimKey selStr =
  mapLeft show $ P.parse pDeprecatedJSPath ("failed to parse role-claim-key value (" <> toS selStr <> ")") (toS selStr)

pDeprecatedJSPath :: P.Parser DeprecatedJSPath
pDeprecatedJSPath = P.many1 pJSPathExp <* P.eof

pJSPathExp :: P.Parser JSPathExp
pJSPathExp = pJSPKey <|> pJSPFilter <|> pJSPIdx

pJSPKey :: P.Parser JSPathExp
pJSPKey = do
  P.char '.'
  val <- toS <$> P.many1 (P.alphaNum <|> P.oneOf "_$@") <|> pQuotedValue
  return (JSPKey val) <?> "pJSPKey: JSPath attribute key"

pJSPIdx :: P.Parser JSPathExp
pJSPIdx = do
  P.char '['
  num <- read <$> P.many1 P.digit
  P.char ']'
  return (JSPIdx num) <?> "pJSPIdx: JSPath array index"

pJSPFilter :: P.Parser JSPathExp
pJSPFilter = do
  P.try $ P.string "[?("
  condition <- pFilterConditionParser
  P.char ')'
  P.char ']'
  P.eof -- this should be the last jspath expression
  return (JSPFilter condition) <?> "pJSPFilter: JSPath filter exp"

pFilterConditionParser :: P.Parser FilterExp
pFilterConditionParser = do
  P.char '@'
  P.spaces
  filt <- matchOperator
  P.spaces
  filt <$> pQuotedValue
    where
      matchOperator =
        P.try (P.string "==^" $> EndsWithCond)
        <|> P.try (P.string "==" $> EqualsCond)
        <|> P.try (P.string "!=" $> NotEqualsCond)
        <|> P.try (P.string "^==" $> StartsWithCond)
        <|> P.try (P.string "*==" $> ContainsCond)

pQuotedValue :: P.Parser Text
pQuotedValue = toS <$> (P.char '"' *> P.many (P.noneOf "\"") <* P.char '"')
