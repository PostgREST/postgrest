module PgQuery where

import RangeQuery
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Search (split)
import qualified Network.HTTP.Types.URI as Net
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Data.Text hiding (map, intersperse, split)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Functor ( (<$>) )
import Data.String.Conversions (cs)
import qualified Data.List as L

data RangedResult = RangedResult {
  rrFrom  :: Int
, rrTo    :: Int
, rrTotal :: Int
, rrBody  :: BS.ByteString
} deriving (Show)

type CompleteQuery = (Query, [Action])
type CompleteQueryT = CompleteQuery -> CompleteQuery
type JsonQuery = CompleteQuery
data QualifiedTable = QualifiedTable {
  qtSchema :: Text
, qtName   :: Text
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
   conjunction = mconcat $ L.intersperse (" and ",[]) (map wherePred cols)

orderT :: [OrderTerm] -> CompleteQueryT
orderT ts q =
  if L.null ts
    then q
    else q <> (" order by ",[]) <> clause
  where
   clause = mconcat $ L.intersperse (", ",[]) (map queryTerm ts)
   queryTerm :: OrderTerm -> CompleteQuery
   queryTerm t =
     (" ? ? ",
       [EscapeIdentifier (otTerm t), Plain (fromByteString $ otDirection t)]
     )
    -- order = fromMaybe "" $ join (lookup "order" qs)
    -- terms = mapMaybe parseOrderTerm $ splitOn "," $ cs order
    -- termPred = mconcat $ L.intersperse ", " (map orderTermSql terms)

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

orderParseTerm :: BS.ByteString -> Maybe OrderTerm
orderParseTerm s =
  case split "." s of
       [d,c] ->
         if d `elem` ["asc", "desc"]
            then Just $ OrderTerm (cs c) $
              if d == "asc" then "asc" else "desc"
            else Nothing
       _ -> Nothing
