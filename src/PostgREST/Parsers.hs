{-|
Module      : PostgREST.Parsers
Description : PostgREST parser combinators

This module is in charge of parsing all the querystring values in an url, e.g. the select, id, order in `/projects?select=id,name&id=eq.1&order=id,name.desc`.
-}
module PostgREST.Parsers where

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

import Data.Either.Combinators (mapLeft)
import Data.Foldable           (foldl1)
import Data.List               (init, last)
import Data.Text               (intercalate, replace, strip)
import Text.Read               (read)

import Data.Tree
import Text.Parsec.Error
import Text.ParserCombinators.Parsec hiding (many, (<|>))

import PostgREST.DbStructureTypes (FieldName)
import PostgREST.Error            (ApiRequestError (ParseRequestError))
import PostgREST.RangeQuery       (NonnegRange)
import PostgREST.Types

import PostgREST.Private.QueryFragment (operators, ftsOperators)

import Protolude                  hiding (intercalate, option,
                                   replace, toS, try)
import Protolude.Conv             (toS)

pRequestSelect :: Text -> Either ApiRequestError [Tree SelectItem]
pRequestSelect selStr =
  mapError $ parse pFieldForest ("failed to parse select parameter (" <> toS selStr <> ")") (toS selStr)

pRequestOnConflict :: Text -> Either ApiRequestError  [FieldName]
pRequestOnConflict oncStr =
  mapError $ parse pColumns ("failed to parse on_conflict parameter (" <> toS oncStr <> ")") (toS oncStr)

pRequestFilter :: (Text, Text) -> Either ApiRequestError (EmbedPath, Filter)
pRequestFilter (k, v) = mapError $ (,) <$> path <*> (Filter <$> fld <*> oper)
  where
    treePath = parse pTreePath ("failed to parser tree path (" ++ toS k ++ ")") $ toS k
    oper = parse (pOpExpr pSingleVal) ("failed to parse filter (" ++ toS v ++ ")") $ toS v
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
    logicTree = do
      op <- snd <$> path
      -- Concat op and v to make pLogicTree argument regular,
      -- in the form of "?and=and(.. , ..)" instead of "?and=(.. , ..)"
      parse pLogicTree ("failed to parse logic tree (" ++ toS v ++ ")") $ toS (op <> v)

pRequestColumns :: Maybe Text -> Either ApiRequestError (Maybe (S.Set FieldName))
pRequestColumns colStr =
  case colStr of
    Just str ->
      mapError $ Just . S.fromList <$> parse pColumns ("failed to parse columns parameter (" <> toS str <> ")") (toS str)
    _ -> Right Nothing

ws :: Parser Text
ws = toS <$> many (oneOf " \t")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

pTreePath :: Parser (EmbedPath, Field)
pTreePath = do
  p <- pFieldName `sepBy1` pDelimiter
  jp <- option [] pJsonPath
  return (init p, (last p, jp))

pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy1` lexeme (char ',')
  where
    pFieldTree :: Parser (Tree SelectItem)
    pFieldTree =  try (Node <$> pRelationSelect <*> between (char '(') (char ')') pFieldForest) <|>
                  Node <$> pFieldSelect <*> pure []

pStar :: Parser Text
pStar = toS <$> (string "*" $> ("*"::ByteString))

pFieldName :: Parser Text
pFieldName =
  pQuotedValue <|>
  intercalate "-" . map toS <$> (many1 (letter <|> digit <|> oneOf "_ ") `sepBy1` dash) <?>
  "field name (* or [a..z0..9_])"
  where
    isDash :: GenParser Char st ()
    isDash = try ( char '-' >> notFollowedBy (char '>') )
    dash :: Parser Char
    dash = isDash $> '-'

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
          pJIdx = JIdx . toS <$> ((:) <$> option '+' (char '-') <*> many1 digit) <* pEnd
          pEnd = try (void $ lookAhead (string "->")) <|>
                 try (void $ lookAhead (string "::")) <|>
                 try eof in
      try pJIdx <|> try pJKey

pField :: Parser Field
pField = lexeme $ (,) <$> pFieldName <*> option [] pJsonPath

aliasSeparator :: Parser ()
aliasSeparator = char ':' >> notFollowedBy (char ':')

pRelationSelect :: Parser SelectItem
pRelationSelect = lexeme $ try ( do
    alias <- optionMaybe ( try(pFieldName <* aliasSeparator) )
    fld <- pField
    hint <- optionMaybe (
        try ( char '!' *> pFieldName) <|>
        -- deprecated, remove in next major version
        try ( char '.' *> pFieldName)
      )
    return (fld, Nothing, alias, hint)
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
    return ((s, []), Nothing, Nothing, Nothing)

pOpExpr :: Parser SingleVal -> Parser OpExpr
pOpExpr pSVal = try ( string "not" *> pDelimiter *> (OpExpr True <$> pOperation)) <|> OpExpr False <$> pOperation
  where
    pOperation :: Parser Operation
    pOperation =
          Op . toS <$> foldl1 (<|>) (try . ((<* pDelimiter) . string) . toS <$> M.keys ops) <*> pSVal
      <|> In <$> (try (string "in" *> pDelimiter) *> pListVal)
      <|> pFts
      <?> "operator (eq, gt, ...)"

    pFts = do
      op   <- foldl1 (<|>) (try . string . toS <$> ftsOps)
      lang <- optionMaybe $ try (between (char '(') (char ')') (many (letter <|> digit <|> oneOf "_")))
      pDelimiter >> Fts (toS op) (toS <$> lang) <$> pSVal

    ops = M.filterWithKey (const . flip notElem ("in":ftsOps)) operators
    ftsOps = M.keys ftsOperators

pSingleVal :: Parser SingleVal
pSingleVal = toS <$> many anyChar

pListVal :: Parser ListVal
pListVal = lexeme (char '(') *> pListElement `sepBy1` char ',' <* lexeme (char ')')

pListElement :: Parser Text
pListElement = try (pQuotedValue <* notFollowedBy (noneOf ",)")) <|> (toS <$> many (noneOf ",)"))

pQuotedValue :: Parser Text
pQuotedValue = toS <$> (char '"' *> many (noneOf "\"") <* char '"')

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

mapError :: Either ParseError a -> Either ApiRequestError a
mapError = mapLeft translateError
  where
    translateError e =
      ParseRequestError message details
      where
        message = show $ errorPos e
        details = strip $ replace "\n" " " $ toS
           $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

-- Used for the config value "role-claim-key"
pRoleClaimKey :: Text -> Either Text JSPath
pRoleClaimKey selStr =
  mapLeft show $ parse pJSPath ("failed to parse role-claim-key value (" <> toS selStr <> ")") (toS selStr)

pJSPath :: Parser JSPath
pJSPath = toJSPath <$> (period *> pPath `sepBy` period <* eof)
  where
    toJSPath :: [(Text, Maybe Int)] -> JSPath
    toJSPath = concatMap (\(key, idx) -> JSPKey key : maybeToList (JSPIdx <$> idx))
    period = char '.' <?> "period (.)"
    pPath :: Parser (Text, Maybe Int)
    pPath = (,) <$> pJSPKey <*> optionMaybe pJSPIdx

pJSPKey :: Parser Text
pJSPKey = toS <$> many1 (alphaNum <|> oneOf "_$@") <|> pQuotedValue <?> "attribute name [a..z0..9_$@])"

pJSPIdx :: Parser Int
pJSPIdx = char '[' *> (read <$> many1 digit) <* char ']' <?> "array index [0..n]"
