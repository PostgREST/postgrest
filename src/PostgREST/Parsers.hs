module PostgREST.Parsers
( parseGetRequest
)
where

import           Control.Applicative hiding ((<$>))
--lines needed for ghc 7.8
import           Data.Functor ((<$>))
import           Data.Traversable (traverse)

import           Control.Monad                 (join)
import           Data.List                     (delete, find)
import           Data.Maybe
import           Data.Monoid
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Data.Tree
import           Network.Wai                   (Request, pathInfo, queryString)
import           PostgREST.Types
import           Text.ParserCombinators.Parsec hiding (many, (<|>))

parseGetRequest :: Request -> Either ParseError ApiRequest
parseGetRequest httpRequest =
  foldr addFilter <$> (addOrder <$> apiRequest <*> ord) <*> flts
  where
    apiRequest = parse (pRequestSelect rootTableName) ("failed to parse select parameter <<"++selectStr++">>") $ cs selectStr
    addOrder (Node r f) o = Node r{order=o} f
    flts = mapM pRequestFilter whereFilters
    rootTableName = cs $ head $ pathInfo httpRequest -- TODO unsafe head
    qString = [(cs k, cs <$> v)|(k,v) <- queryString httpRequest]
    orderStr = join $ lookup "order" qString
    ord = traverse (parse pOrder ("failed to parse order parameter <<"++fromMaybe "" orderStr++">>")) orderStr
    selectStr = fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qString --in case the parametre is missing or empty we default to *
    whereFilters = [ (k, fromJust v) | (k,v) <- qString, k `notElem` ["select", "order"], isJust v ]

pRequestSelect :: Text -> Parser ApiRequest
pRequestSelect rootNodeName = do
  fieldTree <- pFieldForest
  return $ foldr treeEntry (Node (Select rootNodeName [] [] [] Nothing Nothing) []) fieldTree
  where
    treeEntry :: Tree SelectItem -> ApiRequest -> ApiRequest
    treeEntry (Node fld@((fn, _),_) fldForest) (Node rNode rForest) =
      case fldForest of
        [] -> Node (rNode {fields=fld:fields rNode}) rForest
        _  -> Node rNode (foldr treeEntry (Node (Select fn [] [] [] Nothing Nothing) []) fldForest:rForest)

pRequestFilter :: (String, String) -> Either ParseError (Path, Filter)
pRequestFilter (k, v) = (,) <$> path <*> (Filter <$> fld <*> op <*> val)
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ k ++ ")") k
    opVal = parse pOpValueExp ("failed to parse filter (" ++ v ++ ")") v
    path = fst <$> treePath
    fld = snd <$> treePath
    op = fst <$> opVal
    val = snd <$> opVal

addFilter :: (Path, Filter) -> ApiRequest -> ApiRequest
addFilter ([], flt) (Node rn@(Select {filters=flts}) forest) = Node (rn {filters=flt:flts}) forest
addFilter (path, flt) (Node rn forest) =
  case targetNode of
    Nothing -> Node rn forest -- the filter is silenty dropped in the Request does not contain the required path
    Just tn -> Node rn (addFilter (remainingPath, flt) tn:restForest)
  where
    targetNodeName:remainingPath = path
    (targetNode,restForest) = splitForest targetNodeName forest
    splitForest name forst =
      case maybeNode of
        Nothing -> (Nothing,forest)
        Just node -> (Just node, delete node forest)
      where maybeNode = find ((name==).mainTable.rootLabel) forst

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
  where


pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree = try (Node <$> pSelect <*> ( char '(' *> pFieldForest <* char ')'))
      <|>    Node <$> pSelect <*> pure []

pStar :: Parser Text
pStar = cs <$> (string "*" *> pure ("*"::String))

pFieldName :: Parser Text
pFieldName =  cs <$> (many1 (letter <|> digit <|> oneOf "_")
      <?> "field name (* or [a..z0..9_])")

pJsonPathStep :: Parser Text
pJsonPathStep = cs <$> try (string "->" *> pFieldName)

pJsonPath :: Parser [Text]
pJsonPath = (++) <$> many pJsonPathStep <*> ( (:[]) <$> (string "->>" *> pFieldName) )

pField :: Parser Field
pField = lexeme $ (,) <$> pFieldName <*> optionMaybe pJsonPath

pSelect :: Parser SelectItem
pSelect = lexeme $
  try ((,) <$> pField <*>((cs <$>) <$> optionMaybe (string "::" *> many letter)) )
  <|> do
    s <- pStar
    return ((s, Nothing), Nothing)

pOperator :: Parser Operator
pOperator = cs <$> ( try (string "lte") -- has to be before lt
     <|> try (string "lt")
     <|> try (string "eq")
     <|> try (string "gte") -- has to be before gh
     <|> try (string "gt")
     <|> try (string "lt")
     <|> try (string "neq")
     <|> try (string "like")
     <|> try (string "ilike")
     <|> try (string "in")
     <|> try (string "notin")
     <|> try (string "is" )
     <|> try (string "isnot")
     <|> try (string "@@")
     <?> "operator (eq, gt, ...)"
     )

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
