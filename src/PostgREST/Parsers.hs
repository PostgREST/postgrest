module PostgREST.Parsers
-- ( parseGetRequest
-- )
where

import           Prelude
import           Control.Applicative           hiding ((<$>))
import           Data.Monoid
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text, intercalate)
import           Data.Tree
import           PostgREST.QueryBuilder        (operators)
import           PostgREST.Types
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
import           PostgREST.RangeQuery      (NonnegRange,allRange)

pRequestSelect :: Text -> Parser ReadRequest
pRequestSelect rootNodeName = do
  fieldTree <- pFieldForest
  return $ foldr treeEntry (Node (readQuery, (rootNodeName, Nothing, Nothing)) []) fieldTree
  where
    readQuery = Select [] [rootNodeName] [] Nothing allRange
    treeEntry :: Tree SelectItem -> ReadRequest -> ReadRequest
    treeEntry (Node fld@((fn, _),_,alias) fldForest) (Node (q, i) rForest) =
      case fldForest of
        [] -> Node (q {select=fld:select q}, i) rForest
        _  -> Node (q, i) newForest
          where
            newForest =
              foldr treeEntry (Node (Select [] [fn] [] Nothing allRange, (fn, Nothing, alias)) []) fldForest:rForest

pRequestFilter :: (String, String) -> Either ParseError (Path, Filter)
pRequestFilter (k, v) = (,) <$> path <*> (Filter <$> fld <*> op <*> val)
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ k ++ ")") k
    opVal = parse pOpValueExp ("failed to parse filter (" ++ v ++ ")") v
    path = fst <$> treePath
    fld = snd <$> treePath
    op = fst <$> opVal
    val = snd <$> opVal

pRequestOrder :: (String, String) -> Either ParseError (Path, [OrderTerm])
pRequestOrder (k, v) = (,) <$> path <*> ord
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ k ++ ")") k
    path = fst <$> treePath
    ord = parse pOrder ("failed to parse order (" ++ v ++ ")") v

pRequestRange :: (String, NonnegRange) -> Either ParseError (Path, NonnegRange)
pRequestRange (k, v) = (,) <$> path <*> pure v
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ k ++ ")") k
    path = fst <$> treePath

ws :: Parser Text
ws = cs <$> many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

pTreePath :: Parser (Path,Field)
pTreePath = do
  p <- pFieldName `sepBy1` pDelimiter
  jp <- optionMaybe pJsonPath
  let pp = map cs p
      jpp = map cs <$> jp
  return (init pp, (last pp, jpp))

pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree = try (Node <$> pSimpleSelect <*> between (char '{') (char '}') pFieldForest)
          <|>     Node <$> pSelect <*> pure []

pStar :: Parser Text
pStar = cs <$> (string "*" *> pure ("*"::String))


pFieldName :: Parser Text
pFieldName = do
  matches <- (many1 (letter <|> digit <|> oneOf "_") `sepBy1` dash) <?> "field name (* or [a..z0..9_])"
  return $ intercalate "-" $ map cs matches
  where
    isDash :: GenParser Char st ()
    isDash = try ( char '-' >> notFollowedBy (char '>') )
    dash :: Parser Char
    dash = isDash *> pure '-'


pJsonPathStep :: Parser Text
pJsonPathStep = cs <$> try (string "->" *> pFieldName)

pJsonPath :: Parser [Text]
pJsonPath = (++) <$> many pJsonPathStep <*> ( (:[]) <$> (string "->>" *> pFieldName) )

pField :: Parser Field
pField = lexeme $ (,) <$> pFieldName <*> optionMaybe pJsonPath

aliasSeparator :: Parser ()
aliasSeparator = char ':' >> notFollowedBy (char ':')

pSimpleSelect :: Parser SelectItem
pSimpleSelect = lexeme $ try ( do
    alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
    fld <- pField
    return (fld, Nothing, alias)
  )

pSelect :: Parser SelectItem
pSelect = lexeme $
  try (
    do
      alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
      fld <- pField
      cast <- optionMaybe (string "::" *> many letter)
      return (fld, cs <$> cast, alias)
  )
  <|> do
    s <- pStar
    return ((s, Nothing), Nothing, Nothing)

pOperator :: Parser Operator
pOperator = cs <$> (pOp <?> "operator (eq, gt, ...)")
  where pOp = foldl (<|>) empty $ map (try . string . cs . fst) operators

pValue :: Parser FValue
pValue = VText <$> (cs <$> many anyChar)

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOperatiorWithNegation :: Parser Operator
pOperatiorWithNegation = try ( (<>) <$> ( cs <$> string "not." ) <*>  pOperator) <|> pOperator

pOpValueExp :: Parser (Operator, FValue)
pOpValueExp = (,) <$> pOperatiorWithNegation <*> (pDelimiter *> pValue)

pOrder :: Parser [OrderTerm]
pOrder = lexeme pOrderTerm `sepBy` char ','

pOrderTerm :: Parser OrderTerm
pOrderTerm =
  try ( do
    c <- pFieldName
    _ <- pDelimiter
    d <- (string "asc" *> pure OrderAsc)
         <|> (string "desc" *> pure OrderDesc)
    nls <- optionMaybe (pDelimiter *> (
                 try(string "nullslast" *> pure OrderNullsLast)
             <|> try(string "nullsfirst" *> pure OrderNullsFirst)
           ))
    return $ OrderTerm c d nls
  )
  <|> OrderTerm <$> (cs <$> pFieldName) <*> pure OrderAsc <*> pure Nothing
