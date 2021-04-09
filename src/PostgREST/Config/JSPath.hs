{-|
Module      : PostgREST.Types
Description : PostgREST common types and functions used by the rest of the modules
-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.Config.JSPath
  ( JSPath
  , JSPathExp(..)
  , pRoleClaimKey
  ) where

import qualified Text.ParserCombinators.Parsec as P

import Data.Either.Combinators       (mapLeft)
import Text.ParserCombinators.Parsec ((<?>))
import Text.Read                     (read)

import qualified GHC.Show (show)

import Protolude      hiding (toS)
import Protolude.Conv (toS)


-- | full jspath, e.g. .property[0].attr.detail
type JSPath = [JSPathExp]

-- | jspath expression, e.g. .property, .property[0] or ."property-dash"
data JSPathExp
  = JSPKey Text
  | JSPIdx Int

instance Show JSPathExp where
  -- TODO: this needs to be quoted properly for special chars
  show (JSPKey k) = "." <> show k
  show (JSPIdx i) = "[" <> show i <> "]"

-- Used for the config value "role-claim-key"
pRoleClaimKey :: Text -> Either Text JSPath
pRoleClaimKey selStr =
  mapLeft show $ P.parse pJSPath ("failed to parse role-claim-key value (" <> toS selStr <> ")") (toS selStr)

pJSPath :: P.Parser JSPath
pJSPath = toJSPath <$> (period *> pPath `P.sepBy` period <* P.eof)
  where
    toJSPath :: [(Text, Maybe Int)] -> JSPath
    toJSPath = concatMap (\(key, idx) -> JSPKey key : maybeToList (JSPIdx <$> idx))
    period = P.char '.' <?> "period (.)"
    pPath :: P.Parser (Text, Maybe Int)
    pPath = (,) <$> pJSPKey <*> P.optionMaybe pJSPIdx

pJSPKey :: P.Parser Text
pJSPKey = toS <$> P.many1 (P.alphaNum <|> P.oneOf "_$@") <|> pQuotedValue <?> "attribute name [a..z0..9_$@])"

pJSPIdx :: P.Parser Int
pJSPIdx = P.char '[' *> (read <$> P.many1 P.digit) <* P.char ']' <?> "array index [0..n]"

pQuotedValue :: P.Parser Text
pQuotedValue = toS <$> (P.char '"' *> P.many (P.noneOf "\"") <* P.char '"')
