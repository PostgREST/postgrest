--{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module PostgREST.Parsers
-- ( parseGetRequest
-- , pSelect
-- , pField
-- , pRequestSelect
-- )

where
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
--import Text.Parsec.Text
--import Text.Parsec hiding (many, (<|>))
--import Text.Parsec.Prim hiding (many, (<|>))
import           Control.Applicative
--import Control.Monad
--import qualified Data.Text                     as T
import           Data.Tree
import           Network.Wai                   (Request, pathInfo, queryString)
import           PostgREST.Types
--import qualified Data.ByteString.Char8 as C
--import           Control.Monad
--import           Data.Foldable                 (foldrM)
import           Data.List                     (delete, find)
import           Data.Maybe
import           Data.String.Conversions       (cs)
--import qualified Data.ByteString.Char8 as C

--buildRequest :: String -> String -> [(String, String)] -> Either P.ParseError Request
parseGetRequest :: Request -> Either ParseError ApiRequest
parseGetRequest httpRequest =
    foldr addFilter <$> apiRequest <*> flts
    where
        apiRequest = parse (pRequestSelect rootTableName) ("failed to parse select ("++selectStr++")") $ cs selectStr
        flts = mapM pRequestFilter whereFilters
        rootTableName = cs $ head $ pathInfo httpRequest -- TODO unsafe head
        qString = [(cs k, cs <$> v)|(k,v) <- queryString httpRequest]
        selectStr = fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qString --in case the parametre is missing or empty we default to *
        whereFilters = [ (k, fromJust v) | (k,v) <- qString, k `notElem` ["select", "order"], isJust v ]

pRequestSelect :: String -> Parser ApiRequest
pRequestSelect rootNodeName = do
    fieldTree <- pFieldForest
    return $ foldr treeEntry (Node (RequestNode rootNodeName [] []) []) fieldTree
    where
        treeEntry :: Tree SelectItem -> Tree RequestNode -> Tree RequestNode
        treeEntry (Node fld@((fn, _),_) fldForest) (Node rNode rForest) =
            case fldForest of
                [] -> Node (rNode {fields=fld:fields rNode}) rForest
                _  -> Node rNode (foldr treeEntry (Node (RequestNode fn [] []) []) fldForest:rForest)

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

--lexeme :: Parser String -> Parser String
--lexeme :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity a -> Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity a
--lexeme :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity Char -> Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity Char
lexeme p = ws *> p <* ws

pTreePath :: Parser (Path,Field)
pTreePath = do
    p <- (pFieldName `sepBy1` pDelimiter)
    --f <- pField
    jp <- optionMaybe ( string "->" >>  pJsonPath)
    return (init p, (last p, jp))


pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree =
    try ( do
        fld <- pSelect
        char '('
        subforest <- pFieldForest
        char ')'
        return (Node fld subforest)
        )
    <|> do
        fld <- pSelect
        return (Node fld [])

pStar :: Parser String
pStar = string "*" *> pure "*"

pFieldName :: Parser String
pFieldName =  many1 (letter <|> digit <|> oneOf "_")
          <?> "field name (* or [a..z0..9_])"

pJsonPath :: Parser [String]
pJsonPath = pFieldName `sepBy1` (try (string "->>") <|> string "->")

pField :: Parser Field
pField = lexeme $ do
    f <- pFieldName
    jp <- optionMaybe ( (try (string "->>") <|> string "->") >>  pJsonPath)
    return (f, jp)

pSelect :: Parser SelectItem
pSelect = lexeme $
    try (do
        n <- pField
        v <- optionMaybe (string "::" >> many letter)
        return (n, v)
        )
    <|> do
        s <- pStar
        return ((s, Nothing), Nothing)

pOperator :: Parser Operator
pOperator =  try (string "eq")
         <|> try (string "gt")
         <|> try (string "lt")
         <|> try (string "eq")
         <|> try (string "gt")
         <|> try (string "lt")
         <|> try (string "gte")
         <|> try (string "lte")
         <|> try (string "neq")
         <|> try (string "like")
         <|> try (string "ilike")
         <|> try (string "in")
         <|> try (string "notin")
         <|> try (string "is" )
         <|> try (string "isnot")
         <|> try (string "@@")
         <?> "operator (eq, gt, ...)"

pInt :: Parser Int
pInt = try (liftA read (many1 digit)) <?> "integer"

--pValue :: Parser Value
--pValue = (VInt <$> try (pInt <* eof))
--      <|>(VString <$> many anyChar)
pValue :: Parser FValue
pValue = many anyChar

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOpValueExp :: Parser (Operator, FValue)
pOpValueExp = do
  o <- ( try ( liftA2 (++) (string "not.")  pOperator) <|> pOperator )
  pDelimiter
  v <- pValue
  return (o, v)
