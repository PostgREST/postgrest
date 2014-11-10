module PgQuery (
  CompleteQuery
, QualifiedTable(..)
, limitT
, whereT
, orderT
, countRows
, asJsonWithCount
, orderParse
) where

import RangeQuery
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Search (split)
import qualified Network.HTTP.Types.URI as Net
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Data.Monoid
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor ( (<$>) )
import Control.Monad (join)
import Data.String.Conversions (cs)
import qualified Data.List as L

type CompleteQuery = (Query, [Action])
type CompleteQueryT = CompleteQuery -> CompleteQuery
data QualifiedTable = QualifiedTable {
  qtSchema :: BS.ByteString
, qtName   :: BS.ByteString
} deriving (Show)

data OrderTerm = OrderTerm {
  otTerm :: BS.ByteString
, otDirection :: BS.ByteString
}

limitT :: Maybe NonnegRange -> CompleteQueryT
limitT r q =
  q <> (" LIMIT ? OFFSET ? ", [toField limit, toField offset])
  where
    limit  = fromMaybe "ALL" $ show . rangeLimit <$> r
    offset = fromMaybe 0     $ rangeOffset <$> r

whereT :: Net.Query -> CompleteQueryT
whereT params q =
 if L.null params
   then q
   else q <> conjunction
 where
   cols = [ col | col <- params, fst col `notElem` ["order"] ]
   conjunction = mconcat $ L.intersperse andq (map wherePred cols)

orderT :: [OrderTerm] -> CompleteQueryT
orderT ts q =
  if L.null ts
    then q
    else q <> (" order by ",[]) <> clause
  where
   clause = mconcat $ L.intersperse commaq (map queryTerm ts)
   queryTerm :: OrderTerm -> CompleteQuery
   queryTerm t =
     (" ? ? ",
       [EscapeIdentifier (otTerm t), Plain (fromByteString $ otDirection t)]
     )

countRows :: QualifiedTable -> CompleteQuery
countRows t =
  ("select count(1) from ?.?",
   [EscapeIdentifier (qtSchema t), EscapeIdentifier (qtName t)])

asJsonWithCount :: CompleteQueryT
asJsonWithCount (sql, params) = (
    "count(t), array_to_json(array_agg(row_to_json(t))) from (" <> sql <> ") t"
  , params
  )

wherePred :: Net.QueryItem -> CompleteQuery
wherePred (col, predicate) =
  (" ? ? ? ", [EscapeIdentifier col, Plain op, toField value])

  where
    opCode:rest = BS.split '.' $ fromMaybe "." predicate
    value = BS.intercalate "." rest
    op = fromByteString $ case opCode of
                          "eq"  -> "="
                          "gt"  -> ">"
                          "lt"  -> "<"
                          "gte" -> ">="
                          "lte" -> "<="
                          "neq" -> "<>"
                          _     -> "="

orderParse :: Net.Query -> [OrderTerm]
orderParse q =
  mapMaybe orderParseTerm . split "," $ cs order
  where
    order = fromMaybe "" $ join (lookup "order" q)

orderParseTerm :: BS.ByteString -> Maybe OrderTerm
orderParseTerm s =
  case split "." s of
       [d,c] ->
         if d `elem` ["asc", "desc"]
            then Just $ OrderTerm (cs c) $
              if d == "asc" then "asc" else "desc"
            else Nothing
       _ -> Nothing

commaq :: CompleteQuery
commaq  = (", ", [])

andq :: CompleteQuery
andq = (" and ", [])
