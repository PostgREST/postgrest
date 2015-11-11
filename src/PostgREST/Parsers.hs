module PostgREST.Parsers
-- ( parseGetRequest
-- )
where

import           Control.Applicative hiding ((<$>))
import           Data.Monoid
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Data.Tree
import           PostgREST.Types
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
import           PostgREST.PgQuery (operators)


pRequestSelect :: Text -> Parser ApiRequest
pRequestSelect rootNodeName = do
  fieldTree <- pFieldForest
  return $ foldr treeEntry (Node (Select [] [rootNodeName] [] Nothing, (rootNodeName, Nothing)) []) fieldTree
  where
    treeEntry :: Tree SelectItem -> ApiRequest -> ApiRequest
    treeEntry (Node fld@((fn, _),_) fldForest) (Node (q, i) rForest) =
      case fldForest of
        [] -> Node (q {select=fld:select q}, i) rForest
        _  -> Node (q, i) (foldr treeEntry (Node (Select [] [fn] [] Nothing, (fn, Nothing)) []) fldForest:rForest)

pRequestFilter :: (String, String) -> Either ParseError (Path, Filter)
pRequestFilter (k, v) = (,) <$> path <*> (Filter <$> fld <*> op <*> val)
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ k ++ ")") k
    opVal = parse pOpValueExp ("failed to parse filter (" ++ v ++ ")") v
    path = fst <$> treePath
    fld = snd <$> treePath
    op = fst <$> opVal
    val = snd <$> opVal

ws :: Parser Text
ws = cs <$> many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

pTreePath :: Parser (Path,Field)
pTreePath = do
  p <- pFieldName `sepBy1` pDelimiter
  jp <- optionMaybe ( string "->" >>  pJsonPath)
  return (init p, (last p, jp))

pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree = try (Node <$> pSelect <*> between (char '(') (char ')') pFieldForest)
          <|>     Node <$> pSelect <*> pure []

pStar :: Parser Text
pStar = cs <$> (string "*" *> pure ("*"::String))

pFieldName :: Parser Text
pFieldName =  cs <$> (many1 (letter <|> digit <|> oneOf "_")
          <?> "field name (* or [a..z0..9_])")

pJsonPathDelimiter :: Parser Text
pJsonPathDelimiter = cs <$> (try (string "->>") <|> string "->")

pJsonPath :: Parser [Text]
pJsonPath = pFieldName `sepBy1` pJsonPathDelimiter

pField :: Parser Field
pField = lexeme $ (,) <$> pFieldName <*> optionMaybe ( pJsonPathDelimiter *>  pJsonPath)

pSelect :: Parser SelectItem
pSelect = lexeme $
  try ((,) <$> pField <*>((cs <$>) <$> optionMaybe (string "::" *> many letter)) )
  <|> do
    s <- pStar
    return ((s, Nothing), Nothing)

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
    d <- string "asc" <|> string "desc"
    nls <- optionMaybe (pDelimiter *> ( try(string "nullslast" *> pure ("nulls last"::String)) <|> try(string "nullsfirst" *> pure ("nulls first"::String))))
    return $ OrderTerm (cs c) (cs d) (cs <$> nls)
  )
  <|> OrderTerm <$> (cs <$> pFieldName) <*> pure "asc" <*> pure Nothing
