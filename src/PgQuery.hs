{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module PgQuery where

import RangeQuery

import qualified Hasql.Postgres as H
import qualified Hasql.Backend as H

import Data.Text hiding (map)
import Text.Regex.TDFA ( (=~) )
import Text.Regex.TDFA.Text ()
import qualified Network.HTTP.Types.URI as Net
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor ( (<$>) )
import Control.Monad (join)
import Data.String.Conversions (cs)
import qualified Data.Aeson as JSON
import qualified Data.List as L
import Data.Scientific (isInteger, formatScientific, FPFormat(..))

type DynamicSQL = (BS.ByteString, [H.StatementArgument H.Postgres], All)

type StatementT = DynamicSQL -> DynamicSQL

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
  q <> (" LIMIT " <> limit <> " OFFSET " <> offset <> " ", [], mempty)
  where
    limit  = maybe "ALL" (cs . show) $ join $ rangeLimit <$> r
    offset = cs . show $ fromMaybe 0 $ rangeOffset <$> r

whereT :: Net.Query -> StatementT
whereT params q =
 if L.null params
   then q
   else q <> (" where ",[],mempty) <> conjunction
 where
   cols = [ col | col <- params, fst col `notElem` ["order"] ]
   conjunction = mconcat $ L.intersperse andq (map wherePred cols)

orderT :: [OrderTerm] -> StatementT
orderT ts q =
  if L.null ts
    then q
    else q <> (" order by ",[],mempty) <> clause
  where
   clause = mconcat $ L.intersperse commaq (map queryTerm ts)
   queryTerm :: OrderTerm -> DynamicSQL
   queryTerm t = (" " <> cs (pgFmtIdent $ otTerm t) <> " "
                      <> otDirection t              <> " "
                  , [], mempty)

parentheticT :: StatementT
parentheticT (sql, params, pre) =
  (" (" <> sql <> ") ", params, pre)

iffNotT :: DynamicSQL -> StatementT
iffNotT (aq, ap, apre) (bq, bp, bpre) =
  ("WITH aaa AS (" <> aq <> " returning *) " <>
    bq <> " WHERE NOT EXISTS (SELECT * FROM aaa)"
  , ap ++ bp
  , All $ getAll apre && getAll bpre
  )

countRows :: QualifiedTable -> DynamicSQL
countRows t =
  ("select count(1) from " <> fromQt t, [], mempty)

asJsonWithCount :: StatementT
asJsonWithCount (sql, params, pre) = (
    "count(t), array_to_json(array_agg(row_to_json(t)))::character varying from (" <> sql <> ") t"
  , params, pre
  )

asJsonRow :: StatementT
asJsonRow (sql, params, pre) = (
    "row_to_json(t) from (" <> sql <> ") t", params, pre
  )

selectStar :: QualifiedTable -> DynamicSQL
selectStar t =
  ("select * from " <> fromQt t, [], mempty)

insertInto :: QualifiedTable -> [Text] -> [JSON.Value] -> DynamicSQL
insertInto t [] _ =
  ("insert into " <> fromQt t <> " default values returning *", [], mempty)
insertInto t cols vals =
  ("insert into " <> fromQt t <> " (" <>
    cs (intercalate ", " (map pgFmtIdent cols)) <>
    ") values (" <>
    cs (
      intercalate ", " (map
        ((<> "::unknown") . pgFmtLit . unquoted)
        vals)
    ) <> ") returning row_to_json(" <> fromQt t <> ".*)"
  , []
  , mempty
  )

insertSelect :: QualifiedTable -> [Text] -> [JSON.Value] -> DynamicSQL
insertSelect t [] _ =
  ("insert into " <> fromQt t <> " default values returning *", [], mempty)
insertSelect t cols vals =
  ("insert into " <> fromQt t <> " (" <>
    cs (intercalate ", " (map pgFmtIdent cols)) <>
    ") select " <>
    cs (
      intercalate ", " (map
        ((<> "::unknown") . pgFmtLit . unquoted)
        vals)
    )
  , []
  , mempty
  )

update :: QualifiedTable -> [Text] -> [JSON.Value] -> DynamicSQL
update t cols vals =
  ("update " <> fromQt t <> " set (" <>
    cs (intercalate ", " (map pgFmtIdent cols)) <>
    ") = (" <>
    cs (
      intercalate ", " (map
        ((<> "::unknown") . pgFmtLit . unquoted)
        vals)
    ) <> ")"
  , []
  , mempty
  )

wherePred :: Net.QueryItem -> DynamicSQL
wherePred (col, predicate) =
  (" " <> cs (pgFmtIdent $ cs col) <> " " <> op <> " " <> cs (pgFmtLit value) <> "::unknown ", [], mempty)

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

commaq :: DynamicSQL
commaq  = (", ", [], mempty)

andq :: DynamicSQL
andq = (" and ", [], mempty)

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

fromQt :: QualifiedTable -> BS.ByteString
fromQt t = cs $ pgFmtIdent (qtSchema t) <> "." <> pgFmtIdent (qtName t)

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  cs $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = cs . show $ b
unquoted _ = ""

pgParam :: JSON.Value -> H.StatementArgument H.Postgres
pgParam (JSON.Number n) = H.renderValue
  (cs $ formatScientific Fixed
       (if isInteger n then Just 0 else Nothing) n :: Text)
pgParam (JSON.String s) = H.renderValue s
pgParam (JSON.Bool      b) = H.renderValue $
  if b then "t" else "f" :: Text
pgParam JSON.Null       = H.renderValue (Nothing :: Maybe Text)
pgParam (JSON.Object o) = H.renderValue $ JSON.encode o
pgParam (JSON.Array  a) = H.renderValue $ JSON.encode a
