{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}
module PostgREST.Config.JSPath
  ( JSPath
  , JSPathExp(..)
  , FilterExp(..)
  , dumpJSPath
  , pRoleClaimKey
  , walkJSPath
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


-- | full jspath, e.g. .property[0].attr.detail[?(@ == "role1")]
type JSPath = [JSPathExp]

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

dumpJSPath :: JSPathExp -> Text
-- TODO: this needs to be quoted properly for special chars
dumpJSPath (JSPKey k) = "." <> show k
dumpJSPath (JSPIdx i) = "[" <> show i <> "]"
dumpJSPath (JSPFilter cond) = "[?(@" <> expr <> ")]"
  where
    expr =
      case cond of
        EqualsCond text     -> " == " <> show text
        NotEqualsCond text  -> " != " <> show text
        StartsWithCond text -> " ^== " <> show text
        EndsWithCond text   -> " ==^ " <> show text
        ContainsCond text   -> " *== " <> show text

-- | Evaluate JSPath on a JSON
walkJSPath :: Maybe JSON.Value -> JSPath -> Maybe JSON.Value
walkJSPath x                      []                = x
walkJSPath (Just (JSON.Object o)) (JSPKey key:rest) = walkJSPath (KM.lookup (K.fromText key) o) rest
walkJSPath (Just (JSON.Array ar)) (JSPIdx idx:rest) = walkJSPath (ar V.!? idx) rest
walkJSPath (Just (JSON.Array ar)) [JSPFilter jspFilter] = case jspFilter of
    EqualsCond txt     -> findFirstMatch (==) txt ar
    NotEqualsCond txt  -> findFirstMatch (/=) txt ar
    StartsWithCond txt -> findFirstMatch T.isPrefixOf txt ar
    EndsWithCond txt   -> findFirstMatch T.isSuffixOf txt ar
    ContainsCond txt   -> findFirstMatch T.isInfixOf txt ar
  where
    findFirstMatch matchWith pattern = find (\case
      JSON.String txt -> pattern `matchWith` txt
      _               -> False)
walkJSPath _                      _                 = Nothing

-- Used for the config value "role-claim-key"
pRoleClaimKey :: Text -> Either Text JSPath
pRoleClaimKey selStr =
  mapLeft show $ P.parse pJSPath ("failed to parse role-claim-key value (" <> toS selStr <> ")") (toS selStr)

pJSPath :: P.Parser JSPath
pJSPath = P.many1 pJSPathExp <* P.eof

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
