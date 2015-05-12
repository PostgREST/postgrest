{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PgQuery where

import RangeQuery

import qualified Hasql as H
import qualified Hasql.Postgres as P
import qualified Hasql.Backend as B

import qualified Data.Text as T
import Text.Regex.TDFA ( (=~) )
import Text.Regex.TDFA.Text ()
import qualified Network.HTTP.Types.URI as Net
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Vector (empty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor
import Control.Monad (join)
import Data.String.Conversions (cs)
import qualified Data.Aeson as JSON
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Scientific (isInteger, formatScientific, FPFormat(..))

import Prelude

type PStmt = H.Stmt P.Postgres
instance Monoid PStmt where
  mappend (B.Stmt query params prep) (B.Stmt query' params' prep') =
    B.Stmt (query <> query') (params <> params') (prep && prep')
  mempty = B.Stmt "" empty True
type StatementT = PStmt -> PStmt

data QualifiedTable = QualifiedTable {
  qtSchema :: T.Text
, qtName   :: T.Text
} deriving (Show)

data OrderTerm = OrderTerm {
  otTerm :: T.Text
, otDirection :: BS.ByteString
, otNullOrder :: Maybe BS.ByteString
}

limitT :: Maybe NonnegRange -> StatementT
limitT r q =
  q <> B.Stmt (" LIMIT " <> limit <> " OFFSET " <> offset <> " ") empty True
  where
    limit  = maybe "ALL" (cs . show) $ join $ rangeLimit <$> r
    offset = cs . show $ fromMaybe 0 $ rangeOffset <$> r

whereT :: Net.Query -> StatementT
whereT params q =
 if L.null cols
   then q
   else q <> B.Stmt " where " empty True <> conjunction
 where
   cols = [ col | col <- params, fst col `notElem` ["order"] ]
   conjunction = mconcat $ L.intersperse andq (map wherePred cols)

withT :: PStmt -> T.Text -> StatementT
withT (B.Stmt eq ep epre) v (B.Stmt wq wp wpre) =
  B.Stmt ("WITH " <> v <> " AS (" <> eq <> ") " <> wq <> " from " <> v)
    (ep <> wp)
    (epre && wpre)

orderT :: [OrderTerm] -> StatementT
orderT ts q =
  if L.null ts
    then q
    else q <> B.Stmt " order by " empty True <> clause
  where
   clause = mconcat $ L.intersperse commaq (map queryTerm ts)
   queryTerm :: OrderTerm -> PStmt
   queryTerm t = B.Stmt
                   (" " <> cs (pgFmtIdent $ otTerm t) <> " "
                        <> cs (otDirection t)         <> " "
                        <> maybe "" cs (otNullOrder t) <> " ")
                   empty True

parentheticT :: StatementT
parentheticT s =
  s { B.stmtTemplate = " (" <> B.stmtTemplate s <> ") " }

iffNotT :: PStmt -> StatementT
iffNotT (B.Stmt aq ap apre) (B.Stmt bq bp bpre) =
  B.Stmt
    ("WITH aaa AS (" <> aq <> " returning *) " <>
      bq <> " WHERE NOT EXISTS (SELECT * FROM aaa)")
    (ap <> bp)
    (apre && bpre)

countT :: StatementT
countT s =
  s { B.stmtTemplate = "WITH qqq AS (" <> B.stmtTemplate s <> ") SELECT count(1) FROM qqq" }

countRows :: QualifiedTable -> PStmt
countRows t = B.Stmt ("select count(1) from " <> fromQt t) empty True

asJsonWithCount :: StatementT
asJsonWithCount s = s { B.stmtTemplate =
     "count(t), array_to_json(array_agg(row_to_json(t)))::character varying from ("
  <> B.stmtTemplate s <> ") t" }

asJsonRow :: StatementT
asJsonRow s = s { B.stmtTemplate = "row_to_json(t) from (" <> B.stmtTemplate s <> ") t" }

selectStar :: QualifiedTable -> PStmt
selectStar t = B.Stmt ("select * from " <> fromQt t) empty True

returningStarT :: StatementT
returningStarT s = s { B.stmtTemplate = B.stmtTemplate s <> " RETURNING *" }

deleteFrom :: QualifiedTable -> PStmt
deleteFrom t = B.Stmt ("delete from " <> fromQt t) empty True

insertInto :: QualifiedTable
              -> V.Vector T.Text
              -> V.Vector (V.Vector JSON.Value)
              -> PStmt
insertInto t cols vals
  | V.null cols = B.Stmt ("insert into " <> fromQt t <> " default values returning *") empty True
  | otherwise   = B.Stmt
    ("insert into " <> fromQt t <> " (" <>
      T.intercalate ", " (V.toList $ V.map pgFmtIdent cols) <>
      ") values "
      <> T.intercalate ", "
        (V.toList $ V.map (\v -> "("
            <> T.intercalate ", " (V.toList $ V.map insertableValue v)
            <> ")"
          ) vals
        )
      <> " returning row_to_json(" <> fromQt t <> ".*)")
    empty True

insertSelect :: QualifiedTable -> [T.Text] -> [JSON.Value] -> PStmt
insertSelect t [] _ = B.Stmt
  ("insert into " <> fromQt t <> " default values returning *") empty True
insertSelect t cols vals = B.Stmt
  ("insert into " <> fromQt t <> " ("
    <> T.intercalate ", " (map pgFmtIdent cols)
    <> ") select "
    <> T.intercalate ", " (map insertableValue vals))
  empty True

update :: QualifiedTable -> [T.Text] -> [JSON.Value] -> PStmt
update t cols vals = B.Stmt
  ("update " <> fromQt t <> " set ("
    <> T.intercalate ", " (map pgFmtIdent cols)
    <> ") = ("
    <> T.intercalate ", " (map insertableValue vals)
    <> ")")
  empty True

wherePred :: Net.QueryItem -> PStmt
wherePred (col, predicate) =
  B.Stmt (" " <> pgFmtJsonbPath (cs col) <> " " <> op <> " " <>
      if opCode `elem` ["is","isnot"] then whiteList value
                                 else cs sqlValue)
      empty True

  where
    opCode:rest = T.split (=='.') $ cs $ fromMaybe "." predicate
    value = T.intercalate "." rest
    whiteList val = fromMaybe (cs (pgFmtLit val) <> "::unknown ")
                              (L.find ((==) . T.toLower $ val)
                                      ["null","true","false"])
    star c = if c == '*' then '%' else c
    unknownLiteral = (<> "::unknown ") . pgFmtLit

    sqlValue = case opCode of
            "like" -> unknownLiteral $ T.map star value
            "ilike" -> unknownLiteral $ T.map star value
            "in" -> "(" <> T.intercalate ", " (map unknownLiteral $ T.split (==',') value) <> ") "
            _    -> unknownLiteral value

    op = case opCode of
         "eq"  -> "="
         "gt"  -> ">"
         "lt"  -> "<"
         "gte" -> ">="
         "lte" -> "<="
         "neq" -> "<>"
         "like"-> "like"
         "ilike"-> "ilike"
         "in"  -> "in"
         "is"    -> "is"
         "isnot" -> "is not"
         _     -> "="

orderParse :: Net.Query -> [OrderTerm]
orderParse q =
  mapMaybe orderParseTerm . T.split (==',') $ cs order
  where
    order = fromMaybe "" $ join (lookup "order" q)

orderParseTerm :: T.Text -> Maybe OrderTerm
orderParseTerm s =
  case T.split (=='.') s of
       (c:d:nls) ->
         if d `elem` ["asc", "desc"]
            then Just $ OrderTerm c
              ( if d == "asc" then "asc" else "desc" )
              ( case nls of
                  [n] -> if | n == "nullsfirst" -> Just "nulls first"
                            | n == "nullslast"  -> Just "nulls last"
                            | otherwise -> Nothing
                  _   -> Nothing
              )
            else Nothing
       _ -> Nothing

commaq :: PStmt
commaq  = B.Stmt ", " empty True

andq :: PStmt
andq = B.Stmt " and " empty True

data JsonbPath =
    ColIdentifier T.Text
  | KeyIdentifier T.Text
  | SingleArrow JsonbPath JsonbPath
  | DoubleArrow JsonbPath JsonbPath
  deriving (Show)

parseJsonbPath :: T.Text -> Maybe JsonbPath
parseJsonbPath p =
  case T.splitOn "->>" p of
    [a,b] ->
      let i:is = T.splitOn "->" a in
      Just $ DoubleArrow
        (foldl SingleArrow (ColIdentifier i) (map KeyIdentifier is))
        (KeyIdentifier b)
    _ -> Nothing

pgFmtJsonbPath :: T.Text -> T.Text
pgFmtJsonbPath p =
  pgFmtJsonbPath' $ fromMaybe (ColIdentifier p) (parseJsonbPath p)
  where
    pgFmtJsonbPath' (ColIdentifier i) = pgFmtIdent i
    pgFmtJsonbPath' (KeyIdentifier i) = pgFmtLit i
    pgFmtJsonbPath' (SingleArrow a b) =
      pgFmtJsonbPath' a <> "->" <> pgFmtJsonbPath' b
    pgFmtJsonbPath' (DoubleArrow a b) =
      pgFmtJsonbPath' a <> "->>" <> pgFmtJsonbPath' b

pgFmtIdent :: T.Text -> T.Text
pgFmtIdent x =
  let escaped = T.replace "\"" "\"\"" (trimNullChars $ cs x) in
  if escaped =~ danger
    then "\"" <> escaped <> "\""
    else escaped

  where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: T.Text

pgFmtLit :: T.Text -> T.Text
pgFmtLit x =
  let trimmed = trimNullChars x
      escaped = "'" <> T.replace "'" "''" trimmed <> "'"
      slashed = T.replace "\\" "\\\\" escaped in
  cs $ if escaped =~ ("\\\\" :: T.Text)
    then "E" <> slashed
    else slashed

trimNullChars :: T.Text -> T.Text
trimNullChars = T.takeWhile (/= '\x0')

fromQt :: QualifiedTable -> T.Text
fromQt t = pgFmtIdent (qtSchema t) <> "." <> pgFmtIdent (qtName t)

unquoted :: JSON.Value -> T.Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  cs $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = cs . show $ b
unquoted v = cs $ JSON.encode v

insertableText :: T.Text -> T.Text
insertableText = (<> "::unknown") . pgFmtLit

insertableValue :: JSON.Value -> T.Text
insertableValue JSON.Null = "null"
insertableValue v = insertableText $ unquoted v

paramFilter :: JSON.Value -> T.Text
paramFilter JSON.Null = "is.null"
paramFilter v = "eq." <> unquoted v
