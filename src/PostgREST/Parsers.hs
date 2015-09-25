--{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module PostgREST.Parsers
( parseGetRequest
)
where

import           Control.Applicative
import           Control.Monad                 (join)
import           Data.List                     (delete, find)
import           Data.Maybe
import           Data.String.Conversions       (cs)
import           Data.Tree
import           Network.Wai                   (Request, pathInfo, queryString)
import           PostgREST.Types
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
parseGetRequest :: Request -> Either ParseError ApiRequest
parseGetRequest httpRequest =
    foldr addFilter <$> (addOrder <$> apiRequest <*> ord) <*> flts
    where
        apiRequest = parse (pRequestSelect rootTableName) ("failed to parse select ("++selectStr++")") $ cs selectStr
        addOrder (Node r f) o = Node r{order=o} f
        flts = mapM pRequestFilter whereFilters
        rootTableName = cs $ head $ pathInfo httpRequest -- TODO unsafe head
        qString = [(cs k, cs <$> v)|(k,v) <- queryString httpRequest]
        orderStr = join $ lookup "order" qString
        ord = traverse (parse pOrder ("failed to parse order ("++fromMaybe "" orderStr++")")) orderStr
        selectStr = fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qString --in case the parametre is missing or empty we default to *
        whereFilters = [ (k, fromJust v) | (k,v) <- qString, k `notElem` ["select", "order"], isJust v ]

pRequestSelect :: String -> Parser ApiRequest
pRequestSelect rootNodeName = do
    fieldTree <- pFieldForest
    return $ foldr treeEntry (Node (RequestNode rootNodeName [] [] Nothing) []) fieldTree
    where
        treeEntry :: Tree SelectItem -> Tree RequestNode -> Tree RequestNode
        treeEntry (Node fld@((fn, _),_) fldForest) (Node rNode rForest) =
            case fldForest of
                [] -> Node (rNode {fields=fld:fields rNode}) rForest
                _  -> Node rNode (foldr treeEntry (Node (RequestNode fn [] [] Nothing) []) fldForest:rForest)

pRequestFilter :: (String, String) -> Either ParseError (Path, Filter)
pRequestFilter (k, v) = (,) <$> path <*> (Filter <$> fld <*> op <*> val)
    where
        treePath = parse pTreePath ("failed to parser tree path ("++k++")") k
        opVal = parse pOpValueExp ("failed to parse filter ("++v++")") v
        path = fst <$> treePath
        fld = snd <$> treePath
        op = fst <$> opVal
        val = snd <$> opVal

addFilter :: (Path, Filter) -> ApiRequest -> ApiRequest
addFilter ([], flt) (Node rn@(RequestNode {filters=flts}) forest) = Node (rn {filters=flt:flts}) forest
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
            where maybeNode = find ((name==).nodeName.rootLabel) forst

ws :: Parser String
ws = many (oneOf " \t")

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
pFieldTree = try (Node <$> pSelect <*> ( char '(' *> pFieldForest <* char ')'))
          <|>     Node <$> pSelect <*> pure []

pStar :: Parser String
pStar = string "*" *> pure "*"

pFieldName :: Parser String
pFieldName =  many1 (letter <|> digit <|> oneOf "_")
          <?> "field name (* or [a..z0..9_])"

pJsonPathDelimiter :: Parser String
pJsonPathDelimiter = try (string "->>") <|> string "->"

pJsonPath :: Parser [String]
pJsonPath = pFieldName `sepBy1` pJsonPathDelimiter

pField :: Parser Field
pField = lexeme $ (,) <$> pFieldName <*> optionMaybe ( pJsonPathDelimiter *>  pJsonPath)

pSelect :: Parser SelectItem
pSelect = lexeme $
    try ((,) <$> pField <*> optionMaybe (string "::" *> many letter))
    <|> do
        s <- pStar
        return ((s, Nothing), Nothing)

pOperator :: Parser Operator
pOperator =  try (string "lte") -- has to be before lt
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

-- pInt :: Parser Int
-- pInt = try (liftA read (many1 digit)) <?> "integer"

--pValue :: Parser Value
--pValue = (VInt <$> try (pInt <* eof))
--      <|>(VString <$> many anyChar)
pValue :: Parser FValue
pValue = many anyChar

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOperatiorWithNegation :: Parser Operator
pOperatiorWithNegation = try ( (++) <$> string "not." <*>  pOperator) <|> pOperator

pOpValueExp :: Parser (Operator, FValue)
pOpValueExp = (,) <$> pOperatiorWithNegation <*> (pDelimiter *> pValue)

pOrder :: Parser [OrderTerm]
pOrder = lexeme pOrderTerm `sepBy` char ','

pOrderTerm :: Parser OrderTerm
pOrderTerm =  do
  c <- pFieldName
  _ <- pDelimiter
  d <- string "asc" <|> string "desc"
  nls <- optionMaybe (pDelimiter *> ( try(string "nullslast" *> pure ("nulls last"::String)) <|> try(string "nullsfirst" *> pure ("nulls first"::String))))
  return $ OrderTerm (cs c) (cs d) (cs <$> nls)
