{-|
Module      : PostgREST.Types
Description : PostgREST common types and functions used by the rest of the modules
-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.JSPath where

import qualified GHC.Show (show)

import Data.Either.Combinators (mapLeft)
import Text.Read               (read)

import Text.ParserCombinators.Parsec hiding (many, (<|>))

import Protolude      hiding (toS)
import Protolude.Conv (toS)



-- | full jspath, e.g. .property[0].attr.detail
type JSPath = [JSPathExp]
-- | jspath expression, e.g. .property, .property[0] or ."property-dash"
data JSPathExp = JSPKey Text | JSPIdx Int

instance Show JSPathExp where
  -- TODO: this needs to be quoted properly for special chars
  show (JSPKey k) = "." <> show k
  show (JSPIdx i) = "[" <> show i <> "]"

-- Used for the config value "role-claim-key"
pRoleClaimKey :: Text -> Either Text JSPath
pRoleClaimKey selStr =
  mapLeft show $ parse pJSPath ("failed to parse role-claim-key value (" <> toS selStr <> ")") (toS selStr)

pJSPath :: Parser JSPath
pJSPath = toJSPath <$> (period *> pPath `sepBy` period <* eof)
  where
    toJSPath :: [(Text, Maybe Int)] -> JSPath
    toJSPath = concatMap (\(key, idx) -> JSPKey key : maybeToList (JSPIdx <$> idx))
    period = char '.' <?> "period (.)"
    pPath :: Parser (Text, Maybe Int)
    pPath = (,) <$> pJSPKey <*> optionMaybe pJSPIdx

pJSPKey :: Parser Text
pJSPKey = toS <$> many1 (alphaNum <|> oneOf "_$@") <|> pQuotedValue <?> "attribute name [a..z0..9_$@])"

pJSPIdx :: Parser Int
pJSPIdx = char '[' *> (read <$> many1 digit) <* char ']' <?> "array index [0..n]"

pQuotedValue :: Parser Text
pQuotedValue = toS <$> (char '"' *> many (noneOf "\"") <* char '"')
