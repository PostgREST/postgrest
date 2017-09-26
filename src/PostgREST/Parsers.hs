module PostgREST.Parsers where

import           Protolude                     hiding (try, intercalate, replace, option)
import           Control.Monad                 ((>>))
import           Data.Foldable                 (foldl1)
import qualified Data.HashMap.Strict           as M
import           Data.Text                     (intercalate, replace, strip)
import           Data.List                     (init, last)
import           Data.Tree
import           Data.Either.Combinators       (mapLeft)
import           PostgREST.RangeQuery          (NonnegRange,allRange)
import           PostgREST.Types
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
import           Text.Parsec.Error

pRequestSelect :: Text -> Text -> Either ApiRequestError ReadRequest
pRequestSelect rootName selStr =
  mapError $ parse (pReadRequest rootName) ("failed to parse select parameter (" <> toS selStr <> ")") (toS selStr)

pRequestFilter :: (Text, Text) -> Either ApiRequestError (EmbedPath, Filter)
pRequestFilter (k, v) = mapError $ (,) <$> path <*> (Filter <$> fld <*> oper)
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    oper = parse (pOpExpr pSingleVal pListVal) ("failed to parse filter (" ++ toS v ++ ")") $ toS v
    path = fst <$> treePath
    fld = snd <$> treePath

pRequestOrder :: (Text, Text) -> Either ApiRequestError (EmbedPath, [OrderTerm])
pRequestOrder (k, v) = mapError $ (,) <$> path <*> ord'
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    path = fst <$> treePath
    ord' = parse pOrder ("failed to parse order (" ++ toS v ++ ")") $ toS v

pRequestRange :: (ByteString, NonnegRange) -> Either ApiRequestError (EmbedPath, NonnegRange)
pRequestRange (k, v) = mapError $ (,) <$> path <*> pure v
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    path = fst <$> treePath

pRequestLogicTree :: (Text, Text) -> Either ApiRequestError (EmbedPath, LogicTree)
pRequestLogicTree (k, v) = mapError $ (,) <$> embedPath <*> logicTree
  where
    path = parse pLogicPath ("failed to parser logic path (" ++ toS k ++ ")") $ toS k
    embedPath = fst <$> path
    op = snd <$> path
    -- Concat op and v to make pLogicTree argument regular, in the form of "?and=and(.. , ..)" instead of "?and=(.. , ..)"
    logicTree = join $ parse pLogicTree ("failed to parse logic tree (" ++ toS v ++ ")") . toS <$> ((<>) <$> op <*> pure v)

pRequestRpcQParam :: (Text, Text) -> Either ApiRequestError RpcQParam
pRequestRpcQParam (k, v) = mapError $ (,) <$> name <*> val
  where
    name = parse pFieldName ("failed to parse rpc arg name (" ++ toS k ++ ")") $ toS k
    val = toS <$> parse (many anyChar) ("failed to parse rpc arg value (" ++ toS v ++ ")") v

ws :: Parser Text
ws = toS <$> many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

pReadRequest :: Text -> Parser ReadRequest
pReadRequest rootNodeName = do
  fieldTree <- pFieldForest
  return $ foldr treeEntry (Node (readQuery, (rootNodeName, Nothing, Nothing, Nothing)) []) fieldTree
  where
    readQuery = Select [] [rootNodeName] [] Nothing allRange
    treeEntry :: Tree SelectItem -> ReadRequest -> ReadRequest
    treeEntry (Node fld@((fn, _),_,alias,relationDetail) fldForest) (Node (q, i) rForest) =
      case fldForest of
        [] -> Node (q {select=fld:select q}, i) rForest
        _  -> Node (q, i) newForest
          where
            newForest =
              foldr treeEntry (Node (Select [] [fn] [] Nothing allRange, (fn, Nothing, alias, relationDetail)) []) fldForest:rForest

pTreePath :: Parser (EmbedPath, Field)
pTreePath = do
  p <- pFieldName `sepBy1` pDelimiter
  jp <- optionMaybe pJsonPath
  return (init p, (last p, jp))

pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree =  try (Node <$> pRelationSelect <*> between (char '{') (char '}') pFieldForest) -- TODO: "{}" deprecated
          <|> try (Node <$> pRelationSelect <*> between (char '(') (char ')') pFieldForest)
          <|> Node <$> pFieldSelect <*> pure []

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

pRelationSelect :: Parser SelectItem
pRelationSelect = lexeme $ try ( do
    alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
    fld <- pField
    relationDetail <- optionMaybe ( try( char '.' *> pFieldName ) )

    return (fld, Nothing, alias, relationDetail)
  )

pFieldSelect :: Parser SelectItem
pFieldSelect = lexeme $
  try (
    do
      alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
      fld <- pField
      cast' <- optionMaybe (string "::" *> many letter)
      return (fld, toS <$> cast', alias, Nothing)
  )
  <|> do
    s <- pStar
    return ((s, Nothing), Nothing, Nothing, Nothing)

pOpExpr :: Parser SingleVal -> Parser ListVal -> Parser OpExpr
pOpExpr pSVal pLVal = try ( string "not" *> pDelimiter *> (OpExpr True <$> pOperation)) <|> OpExpr False <$> pOperation
  where
    pOperation :: Parser Operation
    pOperation =
          Op . toS <$> foldl1 (<|>) (try . ((<* pDelimiter) . string) . toS <$> M.keys ops) <*> pSVal
      <|> In <$> (string "in" *> pDelimiter *> pLVal)
      <|> pFts
      <?> "operator (eq, gt, ...)"
    pFts = do
      mode <- option Normal $
              try (string (show Phrase) *> pDelimiter *> pure Phrase)
          <|> try (string (show Plain) *> pDelimiter *> pure Plain)

      lang <- try (Just <$> manyTill (letter <|> digit <|> oneOf "_") (try (string ".fts") <|> try (string ".@@")) <* pDelimiter) -- TODO: '@@' deprecated
          <|> try (string "fts" *> pDelimiter) *> pure Nothing
          <|> try (string "@@" *> pDelimiter)  *> pure Nothing -- TODO: '@@' deprecated
      Fts mode (toS <$> lang) <$> pSVal
    ops = M.filterWithKey (const . flip notElem ["in", "fts", "@@"]) operators -- TODO: '@@' deprecated

pSingleVal :: Parser SingleVal
pSingleVal = toS <$> many anyChar

pListVal :: Parser ListVal
pListVal =    try (lexeme (char '(') *> pListElement `sepBy1` char ',' <* lexeme (char ')'))
          <|> lexeme pListElement `sepBy1` char ',' -- TODO: "in.3,4,5" deprecated, parens e.g. "in.(3,4,5)" should be used

pListElement :: Parser Text
pListElement = try pQuotedValue <|> (toS <$> many (noneOf ",)"))

pQuotedValue :: Parser Text
pQuotedValue = toS <$> (char '"' *> many (noneOf "\"") <* char '"' <* notFollowedBy (noneOf ",)"))

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

pLogicTree :: Parser LogicTree
pLogicTree = Stmnt <$> try pLogicFilter
             <|> Expr <$> pNot <*> pLogicOp <*> (lexeme (char '(') *> pLogicTree `sepBy1` lexeme (char ',') <* lexeme (char ')'))
  where
    pLogicFilter :: Parser Filter
    pLogicFilter = Filter <$> pField <* pDelimiter <*> pOpExpr pLogicSingleVal pLogicListVal
    pNot :: Parser Bool
    pNot = try (string "not" *> pDelimiter *> pure True)
           <|> pure False
           <?> "negation operator (not)"
    pLogicOp :: Parser LogicOperator
    pLogicOp = try (string "and"  *> pure And)
               <|> string "or" *> pure Or
               <?> "logic operator (and, or)"

pLogicSingleVal :: Parser SingleVal
pLogicSingleVal = try pQuotedValue <|> try pPgArray <|> (toS <$> many (noneOf ",)"))
  where
    -- TODO: "{}" deprecated, after removal pPgArray can be removed
    pPgArray :: Parser Text
    pPgArray =  do
      a <- string "{"
      b <- many (noneOf "{}")
      c <- string "}"
      toS <$> pure (a ++ b ++ c)

pLogicListVal :: Parser ListVal
pLogicListVal = lexeme (char '(') *> pListElement `sepBy1` char ',' <* lexeme (char ')')

pLogicPath :: Parser (EmbedPath, Text)
pLogicPath = do
  path <- pFieldName `sepBy1` pDelimiter
  let op = last path
      notOp = "not." <> op
  return (filter (/= "not") (init path), if "not" `elem` path then notOp else op)

mapError :: Either ParseError a -> Either ApiRequestError a
mapError = mapLeft translateError
  where
    translateError e =
      ParseRequestError message details
      where
        message = show $ errorPos e
        details = strip $ replace "\n" " " $ toS
           $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)
