module PgQuery where

import RangeQuery
import qualified Hasql as H
import qualified Hasql.Postgres as H
import qualified Hasql.Backend as H
import Data.Text hiding (map)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Search (split)
import qualified Network.HTTP.Types.URI as Net
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Data.Monoid
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor ( (<$>) )
import Control.Monad (join)
import Data.String.Conversions (cs)
import Data.Aeson (Value(..), encode)
import qualified Data.List as L

type StatementT = H.Statement H.Postgres -> H.Statement H.Postgres
data QualifiedTable = QualifiedTable {
  qtSchema :: BS.ByteString
, qtName   :: BS.ByteString
} deriving (Show)

data OrderTerm = OrderTerm {
  otTerm :: BS.ByteString
, otDirection :: BS.ByteString
}

limitT :: Maybe NonnegRange -> StatementT
limitT r q =
  q <> (" LIMIT " <> limit <> " OFFSET " <> (cs . show) offset <> " ", [])
  where
    limit  = cs $ fromMaybe "ALL" $ show . rangeLimit <$> r
    offset = fromMaybe 0     $ rangeOffset <$> r

whereT :: Net.Query -> StatementT
whereT params q =
 if L.null params
   then q
   else q <> (" where ",[]) <> conjunction
 where
   cols = [ col | col <- params, fst col `notElem` ["order"] ]
   conjunction = mconcat $ L.intersperse andq (map wherePred cols)

orderT :: [OrderTerm] -> StatementT
orderT ts q =
  if L.null ts
    then q
    else q <> (" order by ",[]) <> clause
  where
   clause = mconcat $ L.intersperse commaq (map queryTerm ts)
   queryTerm :: OrderTerm -> H.Statement H.Postgres
   queryTerm t = (" " <> (pgFmtIdent $ otTerm t) <> " "
                      <> otDirection t           <> " "
                  , [])

parentheticT :: StatementT
parentheticT (sql, params) =
  (" (" <> sql <> ") ", params)

iffNotT :: H.Statement H.Postgres -> StatementT
iffNotT (aq, ap) (bq, bp) =
  ("WITH aaa AS (" <> aq <> " returning *) " <>
    bq <> "WHERE NOT EXISTS (SELECT * FROM aaa)"
  , ap ++ bp
  )

countRows :: QualifiedTable -> H.Statement H.Postgres
countRows t =
  ("select count(1) from " <> fromQt t, [])

asJsonWithCount :: StatementT
asJsonWithCount (sql, params) = (
    "count(t), array_to_json(array_agg(row_to_json(t)))::character varying  from (" <> sql <> ") t"
  , params
  )

selectStar :: QualifiedTable -> H.Statement H.Postgres
selectStar t =
  ("select * from " <> fromQt t, [])

insertInto :: QualifiedTable -> [BS.ByteString] -> [Value] ->
              H.Statement H.Postgres
insertInto t [] _ =
  ("insert into " <> fromQt t <> " default values returning *", [])
insertInto t cols vals =
  ("insert into " <> fromQt t <> " (" <>
    BS.intercalate ", " (map pgFmtIdent cols) <>
    ") values (" <>
    BS.intercalate ", " (map (const "?") vals) <>
    ") returning *"
  , vals
  )

rawJsonValue :: Value -> BS.ByteString
rawJsonValue (String s) = cs s
rawJsonValue v = cs $ encode v

update :: QualifiedTable -> [BS.ByteString] -> [Value] ->
          H.Statement H.Postgres
update t cols vals =
  ("update " <> fromQt t <> " set (" <>
    BS.intercalate ", " (map pgFmtIdent cols) <>
    ") = (" <>
    BS.intercalate ", " (map (const "?") vals) <> ")"
  , vals
  )

wherePred :: Net.QueryItem -> H.Statement H.Postgres
wherePred (col, predicate) =
  (" " <> pgFmtIdent col <> " " <> op <> " ? ", [value])

  where
    opCode:rest = BS.split '.' $ fromMaybe "." predicate
    value = BS.intercalate "." rest
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
  mapMaybe orderParseTerm . BS.split "," $ cs order
  where
    order = fromMaybe "" $ join (lookup "order" q)

orderParseTerm :: BS.ByteString -> Maybe OrderTerm
orderParseTerm s =
  case BS.split "." s of
       [d,c] ->
         if d `elem` ["asc", "desc"]
            then Just $ OrderTerm (cs c) $
              if d == "asc" then "asc" else "desc"
            else Nothing
       _ -> Nothing

commaq :: H.Statement H.Postgres
commaq  = (", ", [])

andq :: H.Statement H.Postgres
andq = (" and ", [])

pgFmtIdent :: BS.ByteString -> BS.ByteString
pgFmtIdent x =
  let escaped = replace "\"" "\"\"" (trimNullChars $ cs x) in
  cs $ if escaped =~ danger
    then "\"" <> escaped <> "\""
    else escaped

  where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: BS.ByteString

pgFmtLit :: Text -> Text
pgFmtLit x =
  let trimmed = trimNullChars x
      escaped = "'" <> replace "'" "''" trimmed <> "'"
      slashed = replace "\\" "\\\\" escaped in
  if escaped =~ ("\\\\" :: Text)
    then "E" <> slashed
    else slashed

trimNullChars :: Text -> Text
trimNullChars = Data.Text.takeWhile (/= '\x0')

fromQt :: QualifiedTable -> BS.ByteString
fromQt t = pgFmtIdent (qtSchema t) <> "." <> pgFmtIdent (qtName t)
