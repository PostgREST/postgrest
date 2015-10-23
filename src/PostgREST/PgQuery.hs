{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.PgQuery (
  fromQi
, insertableValue
, wrapQuery
, asJson
, callProc
, iffNotT
, update
, insertSelect
, deleteFrom
, asCsvWithCount
, asJsonWithCount
, unquoted

-- format functions
, pgFmtLit
, pgFmtIdent
, pgFmtValue
, pgFmtCondition
, pgFmtColumn
, pgFmtJsonPath
, pgFmtTable
, pgFmtField
, pgFmtSelectItem
, pgFmtAsJsonPath

-- query transformers (to be removed)
, withT
, countT
, returningStarT
, whereT

-- query fragments
, orderF
, countNoneF
, countAllF
, countF
, locationF
, asCsvF
, asJsonSingleF
, asJsonF

, StatementT
) where


import qualified Hasql                   as H
import qualified Hasql.Backend           as B
import qualified Hasql.Postgres          as P
import           PostgREST.RangeQuery
import           PostgREST.Types

import           Control.Monad           (join)
import qualified Data.Aeson              as JSON
import qualified Data.ByteString.Char8   as BS
import           Data.Functor
import qualified Data.HashMap.Strict     as H
import qualified Data.List               as L
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import           Data.Scientific         (FPFormat (..), formatScientific,
                                          isInteger)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           Data.Vector             (empty)
import qualified Network.HTTP.Types.URI  as Net
import           Text.Regex.TDFA         ((=~))

import           Prelude
import qualified Data.Map                as M

type PStmt = H.Stmt P.Postgres
instance Monoid PStmt where
  mappend (B.Stmt query params prep) (B.Stmt query' params' prep') =
    B.Stmt (query <> query') (params <> params') (prep && prep')
  mempty = B.Stmt "" empty True
type StatementT = PStmt -> PStmt
data JsonbPath =
    ColIdentifier T.Text
  | KeyIdentifier T.Text
  | SingleArrow JsonbPath JsonbPath
  | DoubleArrow JsonbPath JsonbPath
  deriving (Show)

operators :: M.Map T.Text T.Text
operators = M.fromList [
  ("eq", "="),
  ("gt", ">"),
  ("lt", "<"),
  ("gte", ">="),
  ("lte", "<="),
  ("neq", "<>"),
  ("like", "like"),
  ("ilike", "ilike"),
  ("in", "in"),
  ("notin", "not in"),
  ("is", "is"),
  ("isnot", "is not"),
  ("@@", "@@")
  ]


whereT :: QualifiedIdentifier -> Net.Query -> StatementT
whereT table params q =
  if L.null cols
    then q
    else q <> B.Stmt " where " empty True <> conjunction
  where
    cols = [ col | col <- params, fst col `notElem` ["order","select"] ]
    wherePredTable = wherePred table
    conjunction = mconcat $ L.intersperse andq (map wherePredTable cols)

withT :: PStmt -> T.Text -> StatementT
withT (B.Stmt eq ep epre) v (B.Stmt wq wp wpre) =
  B.Stmt ("WITH " <> v <> " AS (" <> eq <> ") " <> wq <> " from " <> v)
    (ep <> wp)
    (epre && wpre)

iffNotT :: PStmt -> StatementT
iffNotT (B.Stmt aq ap apre) (B.Stmt bq bp bpre) =
  B.Stmt
    ("WITH aaa AS (" <> aq <> " returning *) " <>
      bq <> " WHERE NOT EXISTS (SELECT * FROM aaa)")
    (ap <> bp)
    (apre && bpre)

countT :: StatementT
countT s =
  s { B.stmtTemplate = "WITH qqq AS (" <> B.stmtTemplate s <> ") SELECT pg_catalog.count(1) FROM qqq" }

asCsvWithCount :: QualifiedIdentifier -> StatementT
asCsvWithCount table = withCount . asCsv table

asCsv :: QualifiedIdentifier -> StatementT
asCsv table s = s {
  B.stmtTemplate =
    "(select string_agg(quote_ident(column_name::text), ',') from "
    <> "(select column_name from information_schema.columns where quote_ident(table_schema) || '.' || table_name = '"
    <> fromQi table <> "' order by ordinal_position) h) || '\r' || "
    <> "coalesce(string_agg(substring(t::text, 2, length(t::text) - 2), '\r'), '') from ("
    <> B.stmtTemplate s <> ") t" }

asJsonWithCount :: StatementT
asJsonWithCount = withCount . asJson

asJson :: StatementT
asJson s = s {
  B.stmtTemplate =
    "array_to_json(array_agg(row_to_json(t)))::character varying from ("
    <> B.stmtTemplate s <> ") t" }

withCount :: StatementT
withCount s = s { B.stmtTemplate = "pg_catalog.count(t), " <> B.stmtTemplate s }

returningStarT :: StatementT
returningStarT s = s { B.stmtTemplate = B.stmtTemplate s <> " RETURNING *" }

deleteFrom :: QualifiedIdentifier -> PStmt
deleteFrom t = B.Stmt ("delete from " <> fromQi t) empty True

insertSelect :: QualifiedIdentifier -> [T.Text] -> [JSON.Value] -> PStmt
insertSelect t [] _ = B.Stmt
  ("insert into " <> fromQi t <> " default values returning *") empty True
insertSelect t cols vals = B.Stmt
  ("insert into " <> fromQi t <> " ("
    <> T.intercalate ", " (map pgFmtIdent cols)
    <> ") select "
    <> T.intercalate ", " (map insertableValue vals))
  empty True

update :: QualifiedIdentifier -> [T.Text] -> [JSON.Value] -> PStmt
update t cols vals = B.Stmt
  ("update " <> fromQi t <> " set ("
    <> T.intercalate ", " (map pgFmtIdent cols)
    <> ") = ("
    <> T.intercalate ", " (map insertableValue vals)
    <> ")")
  empty True

callProc :: QualifiedIdentifier -> JSON.Object -> PStmt
callProc qi params = do
  let args = T.intercalate "," $ map assignment (H.toList params)
  B.Stmt ("select * from " <> fromQi qi <> "(" <> args <> ")") empty True
  where
    assignment (n,v) = pgFmtIdent n <> ":=" <> insertableValue v

wherePred :: QualifiedIdentifier -> Net.QueryItem -> PStmt
wherePred table (col, predicate) =
  B.Stmt (notOp <> " " <> pgFmtJsonbPath table (cs col) <> " " <> op <> " " <>
      if opCode `elem` ["is","isnot"] then whiteList val
                                 else cs sqlValue)
      empty True

  where
    headPredicate:rest = T.split (=='.') $ cs $ fromMaybe "." predicate
    hasNot caseTrue caseFalse = if headPredicate == "not" then caseTrue else caseFalse
    opCode        = hasNot (head rest) headPredicate
    notOp         = hasNot headPredicate ""
    val         = hasNot (T.intercalate "." $ tail rest) (T.intercalate "." rest)
    sqlValue = pgFmtValue opCode val
    op = pgFmtOperator opCode

whiteList :: T.Text -> T.Text
whiteList val = fromMaybe
  (cs (pgFmtLit val) <> "::unknown ")
  (L.find ((==) . T.toLower $ val) ["null","true","false"])

andq :: PStmt
andq = B.Stmt " and " empty True

parseJsonbPath :: T.Text -> Maybe JsonbPath
parseJsonbPath p =
  case T.splitOn "->>" p of
    [a,b] ->
      let i:is = T.splitOn "->" a in
      Just $ DoubleArrow
        (foldl SingleArrow (ColIdentifier i) (map KeyIdentifier is))
        (KeyIdentifier b)
    _ -> Nothing

trimNullChars :: T.Text -> T.Text
trimNullChars = T.takeWhile (/= '\x0')

fromQi :: QualifiedIdentifier -> T.Text
fromQi t = (if s == "" then "" else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

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

wrapQuery :: T.Text -> [T.Text] -> Maybe NonnegRange -> T.Text
wrapQuery source selectColumns range =
  withSourceF source <>
  " SELECT " <>
  T.intercalate ", " selectColumns <>
  " " <>
  fromF ( limitF range )


-- query fragments
withSourceF :: T.Text -> T.Text
withSourceF s = "WITH source AS (" <> s <>")"

countF :: T.Text
countF = "pg_catalog.count(t)"

countAllF :: T.Text
countAllF = "(SELECT pg_catalog.count(1) FROM (SELECT * FROM source) a )"

countNoneF :: T.Text
countNoneF = "null"

asJsonF :: T.Text
asJsonF = "array_to_json(array_agg(row_to_json(t)))::character varying"

asJsonSingleF :: T.Text --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "string_agg(row_to_json(t)::text, ',')::character varying "

asCsvF :: T.Text
asCsvF = asCsvHeaderF <> " || '\r' || " <> asCsvBodyF

asCsvHeaderF :: T.Text
asCsvHeaderF =
  "(SELECT string_agg(a.k, ',')" <>
  "  FROM (" <>
  "    SELECT json_object_keys(r)::TEXT as k" <>
  "    FROM ( " <>
  "      SELECT row_to_json(source) as r from source limit 1" <>
  "    ) s" <>
  "  ) a" <>
  ")"

asCsvBodyF :: T.Text
asCsvBodyF = "coalesce(string_agg(substring(t::text, 2, length(t::text) - 2), '\r'), '')"

fromF :: T.Text -> T.Text
fromF limit = "FROM (SELECT * FROM source " <> limit <> ") t"

limitF :: Maybe NonnegRange -> T.Text
limitF r  = "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" (cs . show) $ join $ rangeLimit <$> r
    offset = cs . show $ fromMaybe 0 $ rangeOffset <$> r

locationF :: [T.Text] -> T.Text
locationF pKeys =
    "(" <>
    " WITH s AS (SELECT row_to_json(source) as r from source limit 1)" <>
    " SELECT string_agg(json_data.key || '=' || coalesce( 'eq.' || json_data.value, 'is.null'), '&')" <>
    " FROM s, json_each_text(s.r) AS json_data" <>
    (
      if null pKeys
      then ""
      else " WHERE json_data.key IN ('" <> T.intercalate "','" pKeys <> "')"
    ) <>
    ")"

orderF :: [OrderTerm] -> T.Text
orderF ts =
  if L.null ts
    then ""
    else "ORDER BY " <> clause
  where
    clause = T.intercalate "," (map queryTerm ts)
    queryTerm :: OrderTerm -> T.Text
    queryTerm t = " "
           <> cs (pgFmtIdent $ otTerm t) <> " "
           <> cs (otDirection t)         <> " "
           <> maybe "" cs (otNullOrder t) <> " "

-- formating functions

pgFmtValue :: T.Text -> T.Text -> T.Text
pgFmtValue opCode val =
 case opCode of
   "like" -> unknownLiteral $ T.map star val
   "ilike" -> unknownLiteral $ T.map star val
   "in" -> "(" <> T.intercalate ", " (map unknownLiteral $ T.split (==',') val) <> ") "
   "notin" -> "(" <> T.intercalate ", " (map unknownLiteral $ T.split (==',') val) <> ") "
   "@@" -> "to_tsquery(" <> unknownLiteral val <> ") "
   _    -> unknownLiteral val
 where
   star c = if c == '*' then '%' else c
   unknownLiteral = (<> "::unknown ") . pgFmtLit

pgFmtOperator :: T.Text -> T.Text
pgFmtOperator opCode = fromMaybe "=" $ M.lookup opCode operators

pgFmtJsonbPath :: QualifiedIdentifier -> T.Text -> T.Text
pgFmtJsonbPath table p =
 pgFmtJsonbPath' $ fromMaybe (ColIdentifier p) (parseJsonbPath p)
 where
   pgFmtJsonbPath' (ColIdentifier i) = fromQi table <> "." <> pgFmtIdent i
   pgFmtJsonbPath' (KeyIdentifier i) = pgFmtLit i
   pgFmtJsonbPath' (SingleArrow a b) =
     pgFmtJsonbPath' a <> "->" <> pgFmtJsonbPath' b
   pgFmtJsonbPath' (DoubleArrow a b) =
     pgFmtJsonbPath' a <> "->>" <> pgFmtJsonbPath' b

pgFmtIdent :: T.Text -> T.Text
pgFmtIdent x =
 let escaped = T.replace "\"" "\"\"" (trimNullChars $ cs x) in
 if (cs escaped :: BS.ByteString) =~ danger
   then "\"" <> escaped <> "\""
   else escaped

 where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: BS.ByteString

pgFmtLit :: T.Text -> T.Text
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> T.replace "'" "''" trimmed <> "'"
     slashed = T.replace "\\" "\\\\" escaped in
 if T.isInfixOf "\\\\" escaped
   then "E" <> slashed
   else slashed

pgFmtCondition :: QualifiedIdentifier -> Filter -> T.Text
pgFmtCondition table (Filter (col,jp) ops val) =
 notOp <> " " <> sqlCol  <> " " <> pgFmtOperator opCode <> " " <>
   if opCode `elem` ["is","isnot"] then whiteList (getInner val) else sqlValue
 where
   headPredicate:rest = T.split (=='.') ops
   hasNot caseTrue caseFalse = if headPredicate == "not" then caseTrue else caseFalse
   opCode      = hasNot (head rest) headPredicate
   notOp       = hasNot headPredicate ""
   sqlCol = case val of
     VText _ -> pgFmtColumn table col <> pgFmtJsonPath jp
     VForeignKey qi _ -> pgFmtColumn qi col
   sqlValue = valToStr val
   getInner v = case v of
     VText s -> s
     _      -> ""
   valToStr v = case v of
     VText s -> pgFmtValue opCode s
     VForeignKey (QualifiedIdentifier s _) (ForeignKey ft fc) -> pgFmtColumn (QualifiedIdentifier s ft) fc

pgFmtColumn :: QualifiedIdentifier -> T.Text -> T.Text
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtJsonPath :: Maybe JsonPath -> T.Text
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtTable :: Table -> T.Text
pgFmtTable Table{tableSchema=s, tableName=n} = fromQi $ QualifiedIdentifier s n

pgFmtField :: QualifiedIdentifier -> Field -> T.Text
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> T.Text
pgFmtSelectItem table (f@(_, jp), Nothing) = pgFmtField table f <> pgFmtAsJsonPath jp
pgFmtSelectItem table (f@(_, jp), Just cast ) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAsJsonPath jp

pgFmtAsJsonPath :: Maybe JsonPath -> T.Text
pgFmtAsJsonPath Nothing = ""
pgFmtAsJsonPath (Just xx) = " AS " <> last xx
