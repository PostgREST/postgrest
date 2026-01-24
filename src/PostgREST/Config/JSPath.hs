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
  = JSPKey Text                      -- .property or ."property-dash"
  | JSPIdx Int                       -- [0]
  | JSPSlice (Maybe Int) (Maybe Int) -- [0:5] or [0:] or [:5] or [:]
  | JSPFilter FilterExp              -- [?(@ == "match")]

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
dumpJSPath (JSPSlice s e) = "[" <> maybe "" show s <> ":" <> maybe "" show e <> "]"
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
walkJSPath (Just (JSON.String str)) (JSPSlice start end:rest) =
  let
    len = T.length str

    norm :: Maybe Int -> Maybe Int -- Normalize negative indices to positive
    norm = fmap (\i -> max 0 $ min len $ if i < 0 then len + i else i)

    s = fromMaybe 0 $ norm start -- normalized start index
    e = fromMaybe len $ norm end -- normalized end index
    slicedString = if s >= e then T.empty else T.take (e-s) $ T.drop s str
  in
    walkJSPath (Just $ JSON.String slicedString) rest

walkJSPath (Just (JSON.Array ar)) (JSPFilter jspFilter:rest) = case jspFilter of
    EqualsCond txt     -> walkJSPath (findFirstMatch (==) txt ar) rest
    NotEqualsCond txt  -> walkJSPath (findFirstMatch (/=) txt ar) rest
    StartsWithCond txt -> walkJSPath (findFirstMatch T.isPrefixOf txt ar) rest
    EndsWithCond txt   -> walkJSPath (findFirstMatch T.isSuffixOf txt ar) rest
    ContainsCond txt   -> walkJSPath (findFirstMatch T.isInfixOf txt ar) rest
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
pJSPathExp = P.try pJSPKey <|> P.try pJSPFilter <|> P.try pJSPIdx <|> pJSPSlice

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

pJSPSlice :: P.Parser JSPathExp
pJSPSlice = do
  P.char '['
  startSign <- P.optionMaybe $ P.char '-'
  startIndex <- P.optionMaybe (read <$> P.many1 P.digit)
  P.char ':'
  endSign <- P.optionMaybe $ P.char '-'
  endIndex <- P.optionMaybe (read <$> P.many1 P.digit)
  P.char ']'
  let start' = if isJust startSign then ((-1) *) <$> startIndex else startIndex
      end'   = if isJust endSign   then ((-1) *) <$> endIndex   else endIndex
  return (JSPSlice start' end') <?> "pJSPSlice: JSPath string slice"

pJSPFilter :: P.Parser JSPathExp
pJSPFilter = do
  P.try $ P.string "[?("
  condition <- pFilterConditionParser
  P.char ')'
  P.char ']'
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
