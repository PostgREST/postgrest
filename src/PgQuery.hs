{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PgQuery where

import RangeQuery

import qualified Hasql.Postgres as H
import qualified Hasql.Backend as H

import Data.Text hiding (map, empty)
import Text.Regex.TDFA ( (=~) )
import Text.Regex.TDFA.Text ()
import qualified Network.HTTP.Types.URI as Net
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Vector (empty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor ( (<$>) )
import Control.Monad (join)
import Data.String.Conversions (cs)
import qualified Data.Aeson as JSON
import qualified Data.List as L
import Data.Scientific (isInteger, formatScientific, FPFormat(..))

type PStmt = H.Stmt H.Postgres
instance Monoid PStmt where
  mappend (H.Stmt query params prep) (H.Stmt query' params' prep') =
    H.Stmt (query <> query') (params <> params') (prep && prep')
  mempty = H.Stmt "" empty True
type StatementT = PStmt -> PStmt

data QualifiedTable = QualifiedTable {
  qtSchema :: Text
, qtName   :: Text
} deriving (Show)

data OrderTerm = OrderTerm {
  otTerm :: Text
, otDirection :: BS.ByteString
}

limitT :: Maybe NonnegRange -> StatementT
limitT r q =
  q <> H.Stmt (" LIMIT " <> limit <> " OFFSET " <> offset <> " ") empty True
  where
    limit  = maybe "ALL" (cs . show) $ join $ rangeLimit <$> r
    offset = cs . show $ fromMaybe 0 $ rangeOffset <$> r

whereT :: Net.Query -> StatementT
whereT params q =
 if L.null cols
   then q
   else q <> H.Stmt " where " empty True <> conjunction
 where
   cols = [ col | col <- params, fst col `notElem` ["order"] ]
   conjunction = mconcat $ L.intersperse andq (map wherePred cols)

orderT :: [OrderTerm] -> StatementT
orderT ts q =
  if L.null ts
    then q
    else q <> H.Stmt " order by " empty True <> clause
  where
   clause = mconcat $ L.intersperse commaq (map queryTerm ts)
   queryTerm :: OrderTerm -> PStmt
   queryTerm t = H.Stmt
                   (" " <> cs (pgFmtIdent $ otTerm t) <> " "
                        <> cs (otDirection t)         <> " ")
                   empty True

parentheticT :: StatementT
parentheticT s =
  s { H.stmtTemplate = " (" <> H.stmtTemplate s <> ") " }

iffNotT :: PStmt -> StatementT
iffNotT (H.Stmt aq ap apre) (H.Stmt bq bp bpre) =
  H.Stmt
    ("WITH aaa AS (" <> aq <> " returning *) " <>
      bq <> " WHERE NOT EXISTS (SELECT * FROM aaa)")
    (ap <> bp)
    (apre && bpre)

countT :: StatementT
countT s =
  s { H.stmtTemplate = "WITH qqq AS (" <> H.stmtTemplate s <> ") SELECT count(1) FROM qqq" }

countRows :: QualifiedTable -> PStmt
countRows t = H.Stmt ("select count(1) from " <> fromQt t) empty True

asJsonWithCount :: StatementT
asJsonWithCount s = s { H.stmtTemplate =
     "count(t), array_to_json(array_agg(row_to_json(t)))::character varying from ("
  <> H.stmtTemplate s <> ") t" }

asJsonRow :: StatementT
asJsonRow s = s { H.stmtTemplate = "row_to_json(t) from (" <> H.stmtTemplate s <> ") t" }

selectStar :: QualifiedTable -> PStmt
selectStar t = H.Stmt ("select * from " <> fromQt t) empty True

returningStarT :: StatementT
returningStarT s = s { H.stmtTemplate = H.stmtTemplate s <> " RETURNING *" }

deleteFrom :: QualifiedTable -> PStmt
deleteFrom t = H.Stmt ("delete from " <> fromQt t) empty True

insertInto :: QualifiedTable -> [Text] -> [JSON.Value] -> PStmt
insertInto t [] _ = H.Stmt
  ("insert into " <> fromQt t <> " default values returning *") empty True
insertInto t cols vals = H.Stmt
  ("insert into " <> fromQt t <> " (" <>
    intercalate ", " (map pgFmtIdent cols) <>
    ") values ("
    <> intercalate ", " (map ((<> "::unknown") . pgFmtLit . unquoted) vals)
    <> ") returning row_to_json(" <> fromQt t <> ".*)")
  empty True

insertSelect :: QualifiedTable -> [Text] -> [JSON.Value] -> PStmt
insertSelect t [] _ = H.Stmt
  ("insert into " <> fromQt t <> " default values returning *") empty True
insertSelect t cols vals = H.Stmt
  ("insert into " <> fromQt t <> " ("
    <> intercalate ", " (map pgFmtIdent cols)
    <> ") select "
    <> intercalate ", " (map ((<> "::unknown") . pgFmtLit . unquoted) vals))
  empty True

update :: QualifiedTable -> [Text] -> [JSON.Value] -> PStmt
update t cols vals = H.Stmt
  ("update " <> fromQt t <> " set ("
    <> intercalate ", " (map pgFmtIdent cols)
    <> ") = ("
    <> intercalate ", " (map ((<> "::unknown") . pgFmtLit . unquoted) vals)
    <> ")")
  empty True

wherePred :: Net.QueryItem -> PStmt
wherePred (col, predicate) = H.Stmt
  (" " <> cs (pgFmtIdent $ cs col) <> " " <> op <> " " <> cs (pgFmtLit value) <> "::unknown ")
  empty True

  where
    opCode:rest = split (=='.') $ cs $ fromMaybe "." predicate
    value = intercalate "." rest
    op = case opCode of
         "eq"  -> "="
         "gt"  -> ">"
         "lt"  -> "<"
         "gte" -> ">="
         "lte" -> "<="
         "neq" -> "<>"
         _     -> "="

orderParse :: Net.Query -> [OrderTerm]
orderParse q =
  mapMaybe orderParseTerm . split (==',') $ cs order
  where
    order = fromMaybe "" $ join (lookup "order" q)

orderParseTerm :: Text -> Maybe OrderTerm
orderParseTerm s =
  case split (=='.') s of
       [d,c] ->
         if d `elem` ["asc", "desc"]
            then Just $ OrderTerm c $
              if d == "asc" then "asc" else "desc"
            else Nothing
       _ -> Nothing

commaq :: PStmt
commaq  = H.Stmt ", " empty True

andq :: PStmt
andq = H.Stmt " and " empty True

pgFmtIdent :: Text -> Text
pgFmtIdent x =
  let escaped = replace "\"" "\"\"" (trimNullChars $ cs x) in
  if escaped =~ danger
    then "\"" <> escaped <> "\""
    else escaped

  where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: Text

pgFmtLit :: Text -> Text
pgFmtLit x =
  let trimmed = trimNullChars x
      escaped = "'" <> replace "'" "''" trimmed <> "'"
      slashed = replace "\\" "\\\\" escaped in
  cs $ if escaped =~ ("\\\\" :: Text)
    then "E" <> slashed
    else slashed

trimNullChars :: Text -> Text
trimNullChars = Data.Text.takeWhile (/= '\x0')

fromQt :: QualifiedTable -> Text
fromQt t = pgFmtIdent (qtSchema t) <> "." <> pgFmtIdent (qtName t)

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  cs $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = cs . show $ b
unquoted _ = ""
