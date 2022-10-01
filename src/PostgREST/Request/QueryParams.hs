-- |
-- Module      : PostgREST.Request.QueryParams
-- Description : Parser for PostgREST Query parameters
--
-- This module is in charge of parsing all the querystring values in an url, e.g.
-- the select, id, order in `/projects?select=id,name&id=eq.1&order=id,name.desc`.
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module PostgREST.Request.QueryParams
  ( parse
  , QueryParams(..)
  , pRequestRange
  ) where

import qualified Data.ByteString.Char8         as BS
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Network.HTTP.Base             as HTTP
import qualified Network.HTTP.Types.URI        as HTTP
import qualified Text.ParserCombinators.Parsec as P

import Control.Arrow                 ((***))
import Data.Either.Combinators       (mapLeft)
import Data.List                     (init, last)
import Data.Ranged.Boundaries        (Boundary (..))
import Data.Ranged.Ranges            (Range (..))
import Data.Tree                     (Tree (..))
import Text.Parsec.Error             (errorMessages,
                                      showErrorMessages)
import Text.Parsec.Prim              (parserFail)
import Text.ParserCombinators.Parsec (GenParser, ParseError, Parser,
                                      anyChar, between, char, digit,
                                      eof, errorPos, letter,
                                      lookAhead, many1, noneOf,
                                      notFollowedBy, oneOf,
                                      optionMaybe, sepBy1, string,
                                      try, (<?>))

import PostgREST.DbStructure.Identifiers (FieldName)
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeGeq, rangeLimit,
                                          rangeOffset, restrictRange)

import PostgREST.Request.Types (EmbedParam (..), EmbedPath, Field,
                                Filter (..), FtsOperator (..),
                                JoinType (..), JsonOperand (..),
                                JsonOperation (..), JsonPath, ListVal,
                                LogicOperator (..), LogicTree (..),
                                OpExpr (..), Operation (..),
                                OrderDirection (..), OrderNulls (..),
                                OrderTerm (..), QPError (..),
                                SelectItem, SimpleOperator (..),
                                SingleVal, TrileanVal (..))

import Protolude hiding (try)


-- $setup
-- Setup for doctests
-- >>> import Text.Pretty.Simple (pPrint)
-- >>> deriving instance Show QPError
-- >>> deriving instance Show TrileanVal
-- >>> deriving instance Show FtsOperator
-- >>> deriving instance Show SimpleOperator
-- >>> deriving instance Show Operation
-- >>> deriving instance Show OpExpr
-- >>> deriving instance Show JsonOperand
-- >>> deriving instance Show JsonOperation
-- >>> deriving instance Show Filter
-- >>> deriving instance Show JoinType

data QueryParams =
  QueryParams
    { qsCanonical      :: ByteString
    -- ^ Canonical representation of the query params, sorted alphabetically
    , qsParams         :: [(Text, Text)]
    -- ^ Parameters for RPC calls
    , qsRanges         :: HM.HashMap Text (Range Integer)
    -- ^ Ranges derived from &limit and &offset params
    , qsOrder          :: [(EmbedPath, [OrderTerm])]
    -- ^ &order parameters for each level
    , qsLogic          :: [(EmbedPath, LogicTree)]
    -- ^ &and and &or parameters used for complex boolean logic
    , qsColumns        :: Maybe (S.Set FieldName)
    -- ^ &columns parameter and payload
    , qsSelect         :: [Tree SelectItem]
    -- ^ &select parameter used to shape the response
    , qsFilters        :: [(EmbedPath, Filter)]
    -- ^ Filters on the result from e.g. &id=e.10
    , qsFiltersRoot    :: [Filter]
    -- ^ Subset of the filters that apply on the root table. These are used on UPDATE/DELETE.
    , qsFiltersNotRoot :: [(EmbedPath, Filter)]
    -- ^ Subset of the filters that do not apply on the root table
    , qsFilterFields   :: S.Set FieldName
    -- ^ Set of fields that filters apply to
    , qsOnConflict     :: Maybe [FieldName]
    -- ^ &on_conflict parameter used to upsert on specific unique keys
    }

-- |
-- Parse query parameters from a query string like "id=eq.1&select=name".
--
-- The canonical representation of the query string has parameters sorted alphabetically:
--
-- >>> qsCanonical <$> parse "a=1&c=3&b=2&d"
-- Right "a=1&b=2&c=3&d="
--
-- 'select' is a reserved parameter that selects the fields to be returned:
--
-- >>> qsSelect <$> parse "select=name,location"
-- Right [Node {rootLabel = (("name",[]),Nothing,Nothing,Nothing,Nothing), subForest = []},Node {rootLabel = (("location",[]),Nothing,Nothing,Nothing,Nothing), subForest = []}]
--
-- Filters are parameters whose value contains an operator, separated by a '.' from its value:
--
-- >>> qsFilters <$> parse "a.b=eq.0"
-- Right [(["a"],Filter {field = ("b",[]), opExpr = OpExpr False (Op OpEqual "0")})]
--
-- If the operator specified in a filter does not exist, parsing the query string fails:
--
-- >>> qsFilters <$> parse "a.b=noop.0"
-- Left (QPError "\"failed to parse filter (noop.0)\" (line 1, column 6)" "unknown single value operator noop")
parse :: ByteString -> Either QPError QueryParams
parse qs =
  QueryParams
    canonical
    params
    ranges
    <$> pRequestOrder `traverse` order
    <*> pRequestLogicTree `traverse` logic
    <*> pRequestColumns columns
    <*> pRequestSelect select
    <*> pRequestFilter `traverse` filters
    <*> (fmap snd <$> (pRequestFilter `traverse` filtersRoot))
    <*> pRequestFilter `traverse` filtersNotRoot
    <*> pure (S.fromList (fst <$> filters))
    <*> sequenceA (pRequestOnConflict <$> onConflict)
  where
    logic = filter (endingIn ["and", "or"] . fst) nonemptyParams
    select = fromMaybe "*" $ lookupParam "select"
    onConflict = lookupParam "on_conflict"
    columns = lookupParam "columns"
    order = filter (endingIn ["order"] . fst) nonemptyParams
    limits = filter (endingIn ["limit"] . fst) nonemptyParams
    -- Replace .offset ending with .limit to be able to match those params later in a map
    offsets = first (replaceLast "limit") <$> filter (endingIn ["offset"] . fst) nonemptyParams
    lookupParam :: Text -> Maybe Text
    lookupParam needle = toS <$> join (L.lookup needle qParams)
    nonemptyParams = mapMaybe (\(k, v) -> (k,) <$> v) qParams

    qString = HTTP.parseQueryReplacePlus True qs

    qParams = [(T.decodeUtf8 k, T.decodeUtf8 <$> v)|(k,v) <- qString]

    canonical =
      BS.pack $ HTTP.urlEncodeVars
        . L.sortOn fst
        . map (join (***) BS.unpack . second (fromMaybe mempty))
        $ qString

    endingIn:: [Text] -> Text -> Bool
    endingIn xx key = lastWord `elem` xx
      where lastWord = L.last $ T.split (== '.') key

    (filters, params) = L.partition isParam filtersAndParams
    isParam (k, v) = isEmbedPath k || hasOperator v || hasFtsOperator v

    filtersAndParams = filter (isFilterOrParam . fst)  nonemptyParams
    isFilterOrParam k = not (endingIn reservedEmbeddable k) && notElem k reserved
    reserved = ["select", "columns", "on_conflict"]
    reservedEmbeddable = ["order", "limit", "offset", "and", "or"]

    (filtersNotRoot, filtersRoot) = L.partition isNotRoot filters
    isNotRoot = flip T.isInfixOf "." . fst

    -- TODO: These checks are redundant to the parsers, should use parsers to differentiate params
    hasOperator val =
      case T.splitOn "." val of
        "not" : _ : _ -> True
        "is" : _      -> True
        "in" : _      -> True
        x : _         -> isJust (operator x) || isJust (ftsOperator x)
        _             -> False

    hasFtsOperator val =
      case T.splitOn "(" val of
        x : _ : _ -> isJust $ ftsOperator x
        _         -> False

    isEmbedPath = T.isInfixOf "."
    replaceLast x s = T.intercalate "." $ L.init (T.split (=='.') s) <> [x]

    ranges :: HM.HashMap Text (Range Integer)
    ranges = HM.unionWith f limitParams offsetParams
      where
        f rl ro = Range (BoundaryBelow o) (BoundaryAbove $ o + l - 1)
          where
            l = fromMaybe 0 $ rangeLimit rl
            o = rangeOffset ro

        limitParams =
          HM.fromList [(k, restrictRange (readMaybe v) allRange) | (k,v) <- limits]

        offsetParams =
          HM.fromList [(k, maybe allRange rangeGeq (readMaybe v)) | (k,v) <- offsets]

operator :: Text -> Maybe SimpleOperator
operator = \case
  "eq"     -> Just OpEqual
  "gte"    -> Just OpGreaterThanEqual
  "gt"     -> Just OpGreaterThan
  "lte"    -> Just OpLessThanEqual
  "lt"     -> Just OpLessThan
  "neq"    -> Just OpNotEqual
  "like"   -> Just OpLike
  "ilike"  -> Just OpILike
  "cs"     -> Just OpContains
  "cd"     -> Just OpContained
  "ov"     -> Just OpOverlap
  "sl"     -> Just OpStrictlyLeft
  "sr"     -> Just OpStrictlyRight
  "nxr"    -> Just OpNotExtendsRight
  "nxl"    -> Just OpNotExtendsLeft
  "adj"    -> Just OpAdjacent
  "match"  -> Just OpMatch
  "imatch" -> Just OpIMatch
  _        -> Nothing

ftsOperator :: Text -> Maybe FtsOperator
ftsOperator = \case
  "fts"   -> Just FilterFts
  "plfts" -> Just FilterFtsPlain
  "phfts" -> Just FilterFtsPhrase
  "wfts"  -> Just FilterFtsWebsearch
  _       -> Nothing


-- PARSERS


pRequestSelect :: Text -> Either QPError [Tree SelectItem]
pRequestSelect selStr =
  mapError $ P.parse pFieldForest ("failed to parse select parameter (" <> toS selStr <> ")") (toS selStr)

pRequestOnConflict :: Text -> Either QPError [FieldName]
pRequestOnConflict oncStr =
  mapError $ P.parse pColumns ("failed to parse on_conflict parameter (" <> toS oncStr <> ")") (toS oncStr)

pRequestFilter :: (Text, Text) -> Either QPError (EmbedPath, Filter)
pRequestFilter (k, v) = mapError $ (,) <$> path <*> (Filter <$> fld <*> oper)
  where
    treePath = P.parse pTreePath ("failed to parse tree path (" ++ toS k ++ ")") $ toS k
    oper = P.parse (pOpExpr pSingleVal) ("failed to parse filter (" ++ toS v ++ ")") $ toS v
    path = fst <$> treePath
    fld = snd <$> treePath

pRequestOrder :: (Text, Text) -> Either QPError (EmbedPath, [OrderTerm])
pRequestOrder (k, v) = mapError $ (,) <$> path <*> ord'
  where
    treePath = P.parse pTreePath ("failed to parse tree path (" ++ toS k ++ ")") $ toS k
    path = fst <$> treePath
    ord' = P.parse pOrder ("failed to parse order (" ++ toS v ++ ")") $ toS v

pRequestRange :: (Text, NonnegRange) -> Either QPError (EmbedPath, NonnegRange)
pRequestRange (k, v) = mapError $ (,) <$> path <*> pure v
  where
    treePath = P.parse pTreePath ("failed to parse tree path (" ++ toS k ++ ")") $ toS k
    path = fst <$> treePath

pRequestLogicTree :: (Text, Text) -> Either QPError (EmbedPath, LogicTree)
pRequestLogicTree (k, v) = mapError $ (,) <$> embedPath <*> logicTree
  where
    path = P.parse pLogicPath ("failed to parse logic path (" ++ toS k ++ ")") $ toS k
    embedPath = fst <$> path
    logicTree = do
      op <- snd <$> path
      -- Concat op and v to make pLogicTree argument regular,
      -- in the form of "?and=and(.. , ..)" instead of "?and=(.. , ..)"
      P.parse pLogicTree ("failed to parse logic tree (" ++ toS v ++ ")") $ toS (op <> v)

pRequestColumns :: Maybe Text -> Either QPError (Maybe (S.Set FieldName))
pRequestColumns colStr =
  case colStr of
    Just str ->
      mapError $ Just . S.fromList <$> P.parse pColumns ("failed to parse columns parameter (" <> toS str <> ")") (toS str)
    _ -> Right Nothing

ws :: Parser Text
ws = toS <$> many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

pTreePath :: Parser (EmbedPath, Field)
pTreePath = do
  p <- pFieldName `sepBy1` pDelimiter
  jp <- P.option [] pJsonPath
  return (init p, (last p, jp))

pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')
  where
    pFieldTree :: Parser (Tree SelectItem)
    pFieldTree =  try (Node <$> pRelationSelect <*> between (char '(') (char ')') pFieldForest) <|>
                  Node <$> pFieldSelect <*> pure []

pStar :: Parser Text
pStar = string "*" $> "*"

pFieldName :: Parser Text
pFieldName =
  pQuotedValue <|>
  T.intercalate "-" . map toS <$> (many1 pIdentifierChar `sepBy1` dash) <?>
  "field name (* or [a..z0..9_])"
  where
    isDash :: GenParser Char st ()
    isDash = try ( char '-' >> notFollowedBy (char '>') )
    dash :: Parser Char
    dash = isDash $> '-'

-- |
-- Parse json operators in select, order and filters
--
-- >>> P.parse pJsonPath "" "->text"
-- Right [JArrow {jOp = JKey {jVal = "text"}}]
--
-- >>> P.parse pJsonPath "" "->1"
-- Right [JArrow {jOp = JIdx {jVal = "+1"}}]
--
-- >>> P.parse pJsonPath "" "->>text"
-- Right [J2Arrow {jOp = JKey {jVal = "text"}}]
--
-- >>> P.parse pJsonPath "" "->>1"
-- Right [J2Arrow {jOp = JIdx {jVal = "+1"}}]
--
-- >>> P.parse pJsonPath "" "->0,other"
-- Right [JArrow {jOp = JIdx {jVal = "+0"}}]
--
-- >>> P.parse pJsonPath "" "->0.desc"
-- Right [JArrow {jOp = JIdx {jVal = "+0"}}]
pJsonPath :: Parser JsonPath
pJsonPath = many pJsonOperation
  where
    pJsonOperation :: Parser JsonOperation
    pJsonOperation = pJsonArrow <*> pJsonOperand

    pJsonArrow   =
      try (string "->>" $> J2Arrow) <|>
      try (string "->" $> JArrow)

    pJsonOperand =
      let pJKey = JKey . toS <$> pFieldName
          pJIdx = JIdx . toS <$> ((:) <$> P.option '+' (char '-') <*> many1 digit) <* pEnd
          pEnd = try (void $ lookAhead (string "->")) <|>
                 try (void $ lookAhead (string "::")) <|>
                 try (void $ lookAhead (string ".")) <|>
                 try (void $ lookAhead (string ",")) <|>
                 try eof in
      try pJIdx <|> try pJKey

pField :: Parser Field
pField = lexeme $ (,) <$> pFieldName <*> P.option [] pJsonPath

aliasSeparator :: Parser ()
aliasSeparator = char ':' >> notFollowedBy (char ':')

pRelationSelect :: Parser SelectItem
pRelationSelect = lexeme $ try ( do
    alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
    fld <- pField
    prm1 <- optionMaybe pEmbedParam
    prm2 <- optionMaybe pEmbedParam
    return (fld, Nothing, alias, embedParamHint prm1 <|> embedParamHint prm2, embedParamJoin prm1 <|> embedParamJoin prm2)
  )
  where
    pEmbedParam :: Parser EmbedParam
    pEmbedParam =
      char '!' *> (
        try (string "left"  $> EPJoinType JTLeft)  <|>
        try (string "inner" $> EPJoinType JTInner) <|>
        try (EPHint <$> pFieldName))
    embedParamHint prm = case prm of
      Just (EPHint hint) -> Just hint
      _                  -> Nothing
    embedParamJoin prm = case prm of
      Just (EPJoinType jt) -> Just jt
      _                    -> Nothing

pFieldSelect :: Parser SelectItem
pFieldSelect = lexeme $
  try (
    do
      alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
      fld <- pField
      cast' <- optionMaybe (string "::" *> many pIdentifierChar)
      return (fld, toS <$> cast', alias, Nothing, Nothing)
  )
  <|> do
    s <- pStar
    return ((s, []), Nothing, Nothing, Nothing, Nothing)

pOpExpr :: Parser SingleVal -> Parser OpExpr
pOpExpr pSVal = try ( string "not" *> pDelimiter *> (OpExpr True <$> pOperation)) <|> OpExpr False <$> pOperation
  where
    pOperation :: Parser Operation
    pOperation = pIn <|> pIs <|> try pFts <|> pOp <?> "operator (eq, gt, ...)"

    pIn = In <$> (try (string "in" *> pDelimiter) *> pListVal)
    pIs = Is <$> (try (string "is" *> pDelimiter) *> pTriVal)

    pOp = do
      opStr <- try (P.manyTill anyChar (try pDelimiter))
      op <- parseMaybe ("unknown single value operator " <> opStr) . operator $ toS opStr
      Op op <$> pSVal

    pTriVal = try (ciString "null"    $> TriNull)
          <|> try (ciString "unknown" $> TriUnknown)
          <|> try (ciString "true"    $> TriTrue)
          <|> try (ciString "false"   $> TriFalse)
          <?> "null or trilean value (unknown, true, false)"

    pFts = do
      opStr <- try (P.many (noneOf ".("))
      op <- parseMaybe ("unknown fts operator " <> opStr) . ftsOperator $ toS opStr
      lang <- optionMaybe $ try (between (char '(') (char ')') $ many pIdentifierChar)
      pDelimiter >> Fts op (toS <$> lang) <$> pSVal

    parseMaybe :: [Char] -> Maybe a -> Parser a
    parseMaybe err Nothing = parserFail err
    parseMaybe _ (Just x)  = pure x

    -- case insensitive char and string
    ciChar :: Char -> GenParser Char state Char
    ciChar c = char c <|> char (toUpper c)
    ciString :: [Char] -> GenParser Char state [Char]
    ciString = traverse ciChar

pSingleVal :: Parser SingleVal
pSingleVal = toS <$> many anyChar

pListVal :: Parser ListVal
pListVal = lexeme (char '(') *> pListElement `sepBy1` char ',' <* lexeme (char ')')

pListElement :: Parser Text
pListElement = try (pQuotedValue <* notFollowedBy (noneOf ",)")) <|> (toS <$> many (noneOf ",)"))

pQuotedValue :: Parser Text
pQuotedValue = toS <$> (char '"' *> many pCharsOrSlashed <* char '"')
  where
    pCharsOrSlashed = noneOf "\\\"" <|> (char '\\' *> anyChar)

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOrder :: Parser [OrderTerm]
pOrder = lexeme pOrderTerm `sepBy1` char ','

pOrderTerm :: Parser OrderTerm
pOrderTerm = do
  fld <- pField
  dir <- optionMaybe $
         try (pDelimiter *> string "asc" $> OrderAsc) <|>
         try (pDelimiter *> string "desc" $> OrderDesc)
  nls <- optionMaybe pNulls <* pEnd <|>
         pEnd $> Nothing
  return $ OrderTerm fld dir nls
  where
    pNulls = try (pDelimiter *> string "nullsfirst" $> OrderNullsFirst) <|>
             try (pDelimiter *> string "nullslast"  $> OrderNullsLast)
    pEnd = try (void $ lookAhead (char ',')) <|>
           try eof

pLogicTree :: Parser LogicTree
pLogicTree = Stmnt <$> try pLogicFilter
             <|> Expr <$> pNot <*> pLogicOp <*> (lexeme (char '(') *> pLogicTree `sepBy1` lexeme (char ',') <* lexeme (char ')'))
  where
    pLogicFilter :: Parser Filter
    pLogicFilter = Filter <$> pField <* pDelimiter <*> pOpExpr pLogicSingleVal
    pNot :: Parser Bool
    pNot = try (string "not" *> pDelimiter $> True)
           <|> pure False
           <?> "negation operator (not)"
    pLogicOp :: Parser LogicOperator
    pLogicOp = try (string "and" $> And)
               <|> string "or" $> Or
               <?> "logic operator (and, or)"

pLogicSingleVal :: Parser SingleVal
pLogicSingleVal = try (pQuotedValue <* notFollowedBy (noneOf ",)")) <|> try pPgArray <|> (toS <$> many (noneOf ",)"))
  where
    pPgArray :: Parser Text
    pPgArray =  do
      a <- string "{"
      b <- many (noneOf "{}")
      c <- string "}"
      pure (toS $ a ++ b ++ c)

pLogicPath :: Parser (EmbedPath, Text)
pLogicPath = do
  path <- pFieldName `sepBy1` pDelimiter
  let op = last path
      notOp = "not." <> op
  return (filter (/= "not") (init path), if "not" `elem` path then notOp else op)

pColumns :: Parser [FieldName]
pColumns = pFieldName `sepBy1` lexeme (char ',')

pIdentifierChar :: Parser Char
pIdentifierChar = letter <|> digit <|> oneOf "_ $"

mapError :: Either ParseError a -> Either QPError a
mapError = mapLeft translateError
  where
    translateError e =
      QPError message details
      where
        message = show $ errorPos e
        details = T.strip $ T.replace "\n" " " $ toS
           $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)
