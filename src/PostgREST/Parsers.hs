module PostgREST.Parsers where

import           Protolude                     hiding (try, intercalate)
import           Control.Monad                ((>>))
import           Data.Foldable                (foldl1)
import           Data.Text                     (intercalate, replace, strip)
import           Data.List                     (init, last)
import           Data.Tree
import           Data.Either.Combinators       (mapLeft)
import           PostgREST.Types
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
import           Text.Read                    (read)
import           PostgREST.RangeQuery      (NonnegRange,allRange)
import           Text.Parsec.Error

pRequestSelect :: Text -> Text -> Either ApiRequestError ReadRequest
pRequestSelect rootName selStr =
  mapError $ parse (pReadRequest rootName) ("failed to parse select parameter (" <> toS selStr <> ")") (toS selStr)

pRequestFilter :: (Text, Text) -> Either ApiRequestError (Path, Filter)
pRequestFilter (k, v) = mapError $ (,) <$> path <*> (Filter <$> fld <*> oper)
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    oper = parse pOperation ("failed to parse filter (" ++ toS v ++ ")") $ toS v
    path = fst <$> treePath
    fld = snd <$> treePath

pRequestOrder :: (Text, Text) -> Either ApiRequestError (Path, [OrderTerm])
pRequestOrder (k, v) = mapError $ (,) <$> path <*> ord'
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    path = fst <$> treePath
    ord' = parse pOrder ("failed to parse order (" ++ toS v ++ ")") $ toS v

pRequestRange :: (ByteString, NonnegRange) -> Either ApiRequestError (Path, NonnegRange)
pRequestRange (k, v) = mapError $ (,) <$> path <*> pure v
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    path = fst <$> treePath

ws :: Parser Text
ws = toS <$> many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

pReadRequest :: Text -> Parser ReadRequest
pReadRequest rootNodeName = do
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

pTreePath :: Parser (Path,Field)
pTreePath = do
  p <- pFieldName `sepBy1` pDelimiter
  jp <- optionMaybe pJsonPath
  return (init p, (last p, jp))

pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree = try (Node <$> pSimpleSelect <*> between (char '{') (char '}') pFieldForest)
          <|>     Node <$> pSelect <*> pure []

pStar :: Parser Text
pStar = toS <$> (string "*" *> pure ("*"::ByteString))


pFieldName :: Parser Text
pFieldName = do
  matches <- (many1 (letter <|> digit <|> oneOf "_") `sepBy1` dash) <?> "field name (* or [a..z0..9_])"
  return $ intercalate "-" $ map toS matches
  where
    isDash :: GenParser Char st ()
    isDash = try ( char '-' >> notFollowedBy (char '>') )
    dash :: Parser Char
    dash = isDash *> pure '-'


pJsonPathStep :: Parser Text
pJsonPathStep = toS <$> try (string "->" *> pFieldName)

pJsonPath :: Parser [Text]
pJsonPath = (<>) <$> many pJsonPathStep <*> ( (:[]) <$> (string "->>" *> pFieldName) )

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
      cast' <- optionMaybe (string "::" *> many letter)
      return (fld, toS <$> cast', alias)
  )
  <|> do
    s <- pStar
    return ((s, Nothing), Nothing, Nothing)

pOperation :: Parser Operation
pOperation = try ( string "not" *> pDelimiter *> (Operation True <$> pExpr)) <|> Operation False <$> pExpr
  where
    pExpr :: Parser (Operator, Operand)
    pExpr =
          ((,) <$> (read <$> foldl1 (<|>) (try . string . show <$> notInOps)) <*> (pDelimiter *> pVText))
      <|> try (string (show In) *> pDelimiter *> ((,) <$> pure In <*> pVTextL))
      <|> try (string (show NotIn) *> pDelimiter *> ((,) <$> pure NotIn <*> pVTextL))
      <?> "operator (eq, gt, ...)"
    notInOps = [Equals .. Contained]

pVText :: Parser Operand
pVText = VText . toS <$> many anyChar

pVTextL :: Parser Operand
pVTextL = VTextL <$> pLValue `sepBy1` char ','
  where
    pLValue :: Parser Text
    pLValue = toS <$> (try (char '"' *> many (noneOf "\"") <* char '"' <* notFollowedBy (noneOf ",") ) <|> many (noneOf ","))

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOrder :: Parser [OrderTerm]
pOrder = lexeme pOrderTerm `sepBy` char ','

pOrderTerm :: Parser OrderTerm
pOrderTerm =
  try ( do
    c <- pField
    d <- optionMaybe (try $ pDelimiter *> (
               try(string "asc" *> pure OrderAsc)
           <|> try(string "desc" *> pure OrderDesc)
         ))
    nls <- optionMaybe (pDelimiter *> (
                 try(string "nullslast" *> pure OrderNullsLast)
             <|> try(string "nullsfirst" *> pure OrderNullsFirst)
           ))
    return $ OrderTerm c d nls
  )
  <|> OrderTerm <$> pField <*> pure Nothing <*> pure Nothing

mapError :: Either ParseError a -> Either ApiRequestError a
mapError = mapLeft translateError
  where
    translateError e =
      ParseRequestError message details
      where
        message = show $ errorPos e
        details = strip $ replace "\n" " " $ toS
           $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)
