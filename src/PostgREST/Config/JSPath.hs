{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module PostgREST.Config.JSPath
  ( JSPath
  , JSPathExp(..)
  , FilterExp(..)
  , dumpJSPath
  , pRoleClaimKey
  ) where

import qualified Text.ParserCombinators.Parsec as P

import Data.Either.Combinators (mapLeft)
import Text.Read               (read)

import Protolude


-- | full jspath, e.g. .property[0].attr.detail[?(@ == "role1")]
type JSPath = [JSPathExp]

-- | jspath expression
data JSPathExp
  = JSPKey Text         -- .property or ."property-dash"
  | JSPIdx Int          -- [0]
  | JSPFilter FilterExp -- [?(@ == "match")], [?(@ ^== "match-prefix")], etc

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
dumpJSPath (JSPFilter cond) = "[?(@" <> expr <> "]"
  where
    expr =
      case cond of
        EqualsCond text     -> " == " <> text
        NotEqualsCond text  -> " != " <> text
        StartsWithCond text -> " ^== " <> text
        EndsWithCond text   -> " $== " <> text
        ContainsCond text   -> " *== " <> text


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
  return $ JSPKey val

pJSPIdx :: P.Parser JSPathExp
pJSPIdx = do
  P.char '['
  num <- read <$> P.many1 P.digit
  P.char ']'
  return $ JSPIdx num

pJSPFilter :: P.Parser JSPathExp
pJSPFilter = do
  P.try $ P.string "[?("
  condition <- pFilterConditionParser
  P.char ')'
  P.char ']'
  P.eof -- this should be the last jspath expression
  return $ JSPFilter condition

pFilterConditionParser :: P.Parser FilterExp
pFilterConditionParser = do
  P.char '@'
  P.spaces
  condOp <- P.choice $ map P.string ["==", "!=", "^==", "$==", "*=="]
  P.spaces
  value <- pQuotedValue
  return $ case condOp of
     "=="  -> EqualsCond value
     "!="  -> NotEqualsCond value
     "^==" -> StartsWithCond value
     "$==" -> EndsWithCond value
     "*==" -> ContainsCond value
     _     -> EqualsCond value -- Impossible case

pQuotedValue :: P.Parser Text
pQuotedValue = toS <$> (P.char '"' *> P.many (P.noneOf "\"") <* P.char '"')
