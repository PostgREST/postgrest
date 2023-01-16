-- |
-- Module      : PostgREST.ApiRequest.QueryParams
-- Description : Parser for PostgREST Query parameters
--
-- This module is in charge of parsing all the querystring values in an url, e.g.
-- the select, id, order in `/projects?select=id,name&id=eq.1&order=id,name.desc`.
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module PostgREST.ApiRequest.QueryParams
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
                                      optionMaybe, sepBy, sepBy1,
                                      string, try, (<?>))

import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeGeq, rangeLimit,
                                          rangeOffset, restrictRange)
import PostgREST.SchemaCache.Identifiers (FieldName)

import PostgREST.ApiRequest.Types (EmbedParam (..), EmbedPath, Field,
                                   Filter (..), FtsOperator (..),
                                   Hint, JoinType (..),
                                   JsonOperand (..),
                                   JsonOperation (..), JsonPath,
                                   ListVal, LogicOperator (..),
                                   LogicTree (..), OpExpr (..),
                                   Operation (..),
                                   OrderDirection (..),
                                   OrderNulls (..), OrderTerm (..),
                                   QPError (..), SelectItem (..),
                                   SimpleOperator (..), SingleVal,
                                   TrileanVal (..))

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
-- >>> deriving instance Show SelectItem
-- >>> deriving instance Show OrderDirection
-- >>> deriving instance Show OrderNulls
-- >>> deriving instance Show OrderTerm
-- >>> deriving instance Show LogicOperator
-- >>> deriving instance Show LogicTree

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
-- Right [Node {rootLabel = SelectField {selField = ("name",[]), selCast = Nothing, selAlias = Nothing}, subForest = []},Node {rootLabel = SelectField {selField = ("location",[]), selCast = Nothing, selAlias = Nothing}, subForest = []}]
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
    <*> pRequestOnConflict `traverse` onConflict
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

-- |
-- Parse select= into a Forest of SelectItems
--
-- >>> P.parse pFieldForest "" "id"
-- Right [Node {rootLabel = SelectField {selField = ("id",[]), selCast = Nothing, selAlias = Nothing}, subForest = []}]
--
-- >>> P.parse pFieldForest "" "client(id)"
-- Right [Node {rootLabel = SelectRelation {selRelation = "client", selAlias = Nothing, selHint = Nothing, selJoinType = Nothing}, subForest = [Node {rootLabel = SelectField {selField = ("id",[]), selCast = Nothing, selAlias = Nothing}, subForest = []}]}]
--
-- >>> P.parse pFieldForest "" "*,client(*,nested(*))"
-- Right [Node {rootLabel = SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing}, subForest = []},Node {rootLabel = SelectRelation {selRelation = "client", selAlias = Nothing, selHint = Nothing, selJoinType = Nothing}, subForest = [Node {rootLabel = SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing}, subForest = []},Node {rootLabel = SelectRelation {selRelation = "nested", selAlias = Nothing, selHint = Nothing, selJoinType = Nothing}, subForest = [Node {rootLabel = SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing}, subForest = []}]}]}]
--
-- >>> P.parse pFieldForest "" "*,...client(*),other(*)"
-- Right [Node {rootLabel = SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing}, subForest = []},Node {rootLabel = SpreadRelation {selRelation = "client", selHint = Nothing, selJoinType = Nothing}, subForest = [Node {rootLabel = SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing}, subForest = []}]},Node {rootLabel = SelectRelation {selRelation = "other", selAlias = Nothing, selHint = Nothing, selJoinType = Nothing}, subForest = [Node {rootLabel = SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing}, subForest = []}]}]
--
-- >>> P.parse pFieldForest "" ""
-- Right []
--
-- >>> P.parse pFieldForest "" "id,clients(name[])"
-- Left (line 1, column 16):
-- unexpected '['
-- expecting letter, digit, "-", "->>", "->", "::", ")", "," or end of input
--
-- >>> P.parse pFieldForest "" "data->>-78xy"
-- Left (line 1, column 11):
-- unexpected 'x'
-- expecting digit, "->", "::", ".", "," or end of input
pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy` lexeme (char ',')
  where
    pFieldTree =  Node <$> try pSpreadRelationSelect <*> between (char '(') (char ')') pFieldForest <|>
                  Node <$> try pRelationSelect       <*> between (char '(') (char ')') pFieldForest <|>
                  Node <$> pFieldSelect <*> pure []

-- |
-- Parse field names
--
-- >>> P.parse pFieldName "" "identifier"
-- Right "identifier"
--
-- >>> P.parse pFieldName "" "identifier with spaces"
-- Right "identifier with spaces"
--
-- >>> P.parse pFieldName "" "identifier-with-dashes"
-- Right "identifier-with-dashes"
--
-- >>> P.parse pFieldName "" "123"
-- Right "123"
--
-- >>> P.parse pFieldName "" "_"
-- Right "_"
--
-- >>> P.parse pFieldName "" "$"
-- Right "$"
--
-- >>> P.parse pFieldName "" ":"
-- Left (line 1, column 1):
-- unexpected ":"
-- expecting field name (* or [a..z0..9_$])
--
-- >>> P.parse pFieldName "" "\":\""
-- Right ":"
--
-- >>> P.parse pFieldName "" " no leading or trailing spaces "
-- Right "no leading or trailing spaces"
--
-- >>> P.parse pFieldName "" "\" leading and trailing spaces \""
-- Right " leading and trailing spaces "
pFieldName :: Parser Text
pFieldName =
  pQuotedValue <|>
  T.intercalate "-" . map toS <$> (pIdentifier `sepBy1` dash) <?>
  "field name (* or [a..z0..9_$])"
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
--
-- Fails on badly formed negatives
--
-- >>> P.parse pJsonPath "" "->>-78xy"
-- Left (line 1, column 7):
-- unexpected 'x'
-- expecting digit, "->", "::", ".", "," or end of input
--
-- >>> P.parse pJsonPath "" "->>--34"
-- Left (line 1, column 5):
-- unexpected "-"
-- expecting digit
--
-- >>> P.parse pJsonPath "" "->>-xy-4"
-- Left (line 1, column 5):
-- unexpected "x"
-- expecting digit
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

-- |
-- Parse regular fields in select
--
-- >>> P.parse pRelationSelect "" "rel(*)"
-- Right (SelectRelation {selRelation = "rel", selAlias = Nothing, selHint = Nothing, selJoinType = Nothing})
--
-- >>> P.parse pRelationSelect "" "alias:rel(*)"
-- Right (SelectRelation {selRelation = "rel", selAlias = Just "alias", selHint = Nothing, selJoinType = Nothing})
--
-- >>> P.parse pRelationSelect "" "rel!hint(*)"
-- Right (SelectRelation {selRelation = "rel", selAlias = Nothing, selHint = Just "hint", selJoinType = Nothing})
--
-- >>> P.parse pRelationSelect "" "rel!inner(*)"
-- Right (SelectRelation {selRelation = "rel", selAlias = Nothing, selHint = Nothing, selJoinType = Just JTInner})
--
-- >>> P.parse pRelationSelect "" "rel!hint!inner(*)"
-- Right (SelectRelation {selRelation = "rel", selAlias = Nothing, selHint = Just "hint", selJoinType = Just JTInner})
--
-- >>> P.parse pRelationSelect "" "alias:rel!inner!hint(*)"
-- Right (SelectRelation {selRelation = "rel", selAlias = Just "alias", selHint = Just "hint", selJoinType = Just JTInner})
--
-- >>> P.parse pRelationSelect "" "rel->jsonpath(*)"
-- Left (line 1, column 6):
-- unexpected '>'
--
-- >>> P.parse pRelationSelect "" "rel->jsonpath!hint(*)"
-- Left (line 1, column 6):
-- unexpected '>'
pRelationSelect :: Parser SelectItem
pRelationSelect = lexeme $ do
    alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
    name <- pFieldName
    (hint, jType) <- pEmbedParams
    try (void $ lookAhead (string "("))
    return $ SelectRelation name alias hint jType

-- |
-- Parse regular fields in select
--
-- >>> P.parse pFieldSelect "" "name"
-- Right (SelectField {selField = ("name",[]), selCast = Nothing, selAlias = Nothing})
--
-- >>> P.parse pFieldSelect "" "name->jsonpath"
-- Right (SelectField {selField = ("name",[JArrow {jOp = JKey {jVal = "jsonpath"}}]), selCast = Nothing, selAlias = Nothing})
--
-- >>> P.parse pFieldSelect "" "name::cast"
-- Right (SelectField {selField = ("name",[]), selCast = Just "cast", selAlias = Nothing})
--
-- >>> P.parse pFieldSelect "" "alias:name"
-- Right (SelectField {selField = ("name",[]), selCast = Nothing, selAlias = Just "alias"})
--
-- >>> P.parse pFieldSelect "" "alias:name->jsonpath::cast"
-- Right (SelectField {selField = ("name",[JArrow {jOp = JKey {jVal = "jsonpath"}}]), selCast = Just "cast", selAlias = Just "alias"})
--
-- >>> P.parse pFieldSelect "" "*"
-- Right (SelectField {selField = ("*",[]), selCast = Nothing, selAlias = Nothing})
--
-- >>> P.parse pFieldSelect "" "name!hint"
-- Left (line 1, column 5):
-- unexpected '!'
-- expecting letter, digit, "-", "->>", "->", "::", ")", "," or end of input
--
-- >>> P.parse pFieldSelect "" "*!hint"
-- Left (line 1, column 2):
-- unexpected '!'
-- expecting ")", "," or end of input
--
-- >>> P.parse pFieldSelect "" "name::"
-- Left (line 1, column 7):
-- unexpected end of input
-- expecting letter or digit
pFieldSelect :: Parser SelectItem
pFieldSelect = lexeme $ try (do
    s <- pStar
    pEnd
    return $ SelectField (s, []) Nothing Nothing)
  <|> do
    alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
    fld <- pField
    cast' <- optionMaybe (string "::" *> pIdentifier)
    pEnd
    return $ SelectField fld (toS <$> cast') alias
  where
    pEnd = try (void $ lookAhead (string ")")) <|>
           try (void $ lookAhead (string ",")) <|>
           try eof
    pStar = string "*" $> "*"


-- |
-- Parse spread relations in select
--
-- >>> P.parse pSpreadRelationSelect "" "...rel(*)"
-- Right (SpreadRelation {selRelation = "rel", selHint = Nothing, selJoinType = Nothing})
--
-- >>> P.parse pSpreadRelationSelect "" "...rel!hint!inner(*)"
-- Right (SpreadRelation {selRelation = "rel", selHint = Just "hint", selJoinType = Just JTInner})
--
-- >>> P.parse pSpreadRelationSelect "" "rel(*)"
-- Left (line 1, column 1):
-- unexpected "r"
-- expecting "..."
--
-- >>> P.parse pSpreadRelationSelect "" "alias:...rel(*)"
-- Left (line 1, column 1):
-- unexpected "a"
-- expecting "..."
--
-- >>> P.parse pSpreadRelationSelect "" "...rel->jsonpath(*)"
-- Left (line 1, column 9):
-- unexpected '>'
pSpreadRelationSelect :: Parser SelectItem
pSpreadRelationSelect = lexeme $ do
    name <- string "..." >> pFieldName
    (hint, jType) <- pEmbedParams
    try (void $ lookAhead (string "("))
    return $ SpreadRelation name hint jType

pEmbedParams :: Parser (Maybe Hint, Maybe JoinType)
pEmbedParams = do
  prm1 <- optionMaybe pEmbedParam
  prm2 <- optionMaybe pEmbedParam
  return (embedParamHint prm1 <|> embedParamHint prm2, embedParamJoin prm1 <|> embedParamJoin prm2)
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

-- |
-- Parse operator expression used in horizontal filtering
--
-- >>> P.parse (pOpExpr pSingleVal) "" "fts().value"
-- Left (line 1, column 7):
-- unknown single value operator fts()
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
      lang <- optionMaybe $ try (between (char '(') (char ')') pIdentifier)
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

-- |
-- Parses the elements in the order query parameter
--
-- >>> P.parse pOrder "" "name.desc.nullsfirst"
-- Right [OrderTerm {otTerm = ("name",[]), otDirection = Just OrderDesc, otNullOrder = Just OrderNullsFirst}]
--
-- >>> P.parse pOrder "" "json_col->key.asc.nullslast"
-- Right [OrderTerm {otTerm = ("json_col",[JArrow {jOp = JKey {jVal = "key"}}]), otDirection = Just OrderAsc, otNullOrder = Just OrderNullsLast}]
--
-- >>> P.parse pOrder "" "clients(json_col->key).desc.nullsfirst"
-- Right [OrderRelationTerm {otRelation = "clients", otRelTerm = ("json_col",[JArrow {jOp = JKey {jVal = "key"}}]), otDirection = Just OrderDesc, otNullOrder = Just OrderNullsFirst}]
--
-- >>> P.parse pOrder "" "clients(name,id)"
-- Left (line 1, column 8):
-- unexpected '('
-- expecting letter, digit, "-", "->>", "->", delimiter (.), "," or end of input
--
-- >>> P.parse pOrder "" "name,clients(name),id"
-- Right [OrderTerm {otTerm = ("name",[]), otDirection = Nothing, otNullOrder = Nothing},OrderRelationTerm {otRelation = "clients", otRelTerm = ("name",[]), otDirection = Nothing, otNullOrder = Nothing},OrderTerm {otTerm = ("id",[]), otDirection = Nothing, otNullOrder = Nothing}]
--
-- >>> P.parse pOrder "" "id.ac"
-- Left (line 1, column 4):
-- unexpected "c"
-- expecting "asc", "desc", "nullsfirst" or "nullslast"
--
-- >>> P.parse pOrder "" "id.descc"
-- Left (line 1, column 8):
-- unexpected 'c'
-- expecting delimiter (.), "," or end of input
--
-- >>> P.parse pOrder "" "id.nulsfist"
-- Left (line 1, column 4):
-- unexpected "n"
-- expecting "asc", "desc", "nullsfirst" or "nullslast"
--
-- >>> P.parse pOrder "" "id.nullslasttt"
-- Left (line 1, column 13):
-- unexpected 't'
-- expecting "," or end of input
--
-- >>> P.parse pOrder "" "id.smth34"
-- Left (line 1, column 4):
-- unexpected "s"
-- expecting "asc", "desc", "nullsfirst" or "nullslast"
--
-- >>> P.parse pOrder "" "id.asc.nlsfst"
-- Left (line 1, column 8):
-- unexpected "l"
-- expecting "nullsfirst" or "nullslast"
--
-- >>> P.parse pOrder "" "id.asc.nullslasttt"
-- Left (line 1, column 17):
-- unexpected 't'
-- expecting "," or end of input
--
-- >>> P.parse pOrder "" "id.asc.smth34"
-- Left (line 1, column 8):
-- unexpected "s"
-- expecting "nullsfirst" or "nullslast"
pOrder :: Parser [OrderTerm]
pOrder = lexeme (try pOrderRelationTerm <|> pOrderTerm) `sepBy1` char ','
  where
    pOrderTerm = do
      fld <- pField
      dir <- optionMaybe pOrdDir
      nls <- optionMaybe pNulls <* pEnd <|>
             pEnd $> Nothing
      return $ OrderTerm fld dir nls

    pOrderRelationTerm = do
      nam <- pFieldName
      fld <- between (char '(') (char ')') pField
      dir <- optionMaybe pOrdDir
      nls <- optionMaybe pNulls <* pEnd <|> pEnd $> Nothing
      return $ OrderRelationTerm nam fld dir nls

    pNulls :: Parser OrderNulls
    pNulls = try (pDelimiter *> string "nullsfirst" $> OrderNullsFirst) <|>
             try (pDelimiter *> string "nullslast"  $> OrderNullsLast)

    pOrdDir :: Parser OrderDirection
    pOrdDir = try (pDelimiter *> string "asc" $> OrderAsc) <|>
              try (pDelimiter *> string "desc" $> OrderDesc)

    pEnd = try (void $ lookAhead (char ',')) <|> try eof

-- |
-- Parses the elements inside or/and
--
-- >>> P.parse pLogicTree "" "or()"
-- Left (line 1, column 4):
-- unexpected ")"
-- expecting field name (* or [a..z0..9_$]), negation operator (not) or logic operator (and, or)
--
-- >>> P.parse pLogicTree "" "or(id.in.1,2,id.eq.3)"
-- Left (line 1, column 10):
-- unexpected "1"
-- expecting "("
--
-- >>> P.parse pLogicTree "" "or)("
-- Left (line 1, column 3):
-- unexpected ")"
-- expecting "("
--
-- >>> P.parse pLogicTree "" "and(ord(id.eq.1,id.eq.1),id.eq.2)"
-- Left (line 1, column 7):
-- unexpected "d"
-- expecting "("
--
-- >>> P.parse pLogicTree "" "or(id.eq.1,not.xor(id.eq.2,id.eq.3))"
-- Left (line 1, column 16):
-- unexpected "x"
-- expecting logic operator (and, or)
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

pIdentifier :: Parser Text
pIdentifier = T.strip . toS <$> many1 pIdentifierChar

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
