{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : PostgREST.QueryBuilder
Description : PostgREST SQL generating functions.

This module provides functions to consume data types that
represent database objects (e.g. Relation, Schema, SqlQuery)
and produces SQL Statements.

Any function that outputs a SQL fragment should be in this module.
-}
module PostgREST.QueryBuilder (
    callProc
  , createReadStatement
  , createWriteStatement
  , getJoinFilters
  , pgFmtIdent
  , pgFmtLit
  , requestToQuery
  , requestToCountQuery
  , sourceCTEName
  , unquoted
  , ResultsWithCount
  , pgFmtEnvVar
  ) where

import qualified Hasql.Query             as H
import qualified Hasql.Encoders          as HE
import qualified Hasql.Decoders          as HD

import qualified Data.Aeson              as JSON

import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset, allRange)
import           Data.Functor.Contravariant (contramap)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import           Data.Text               (intercalate, unwords, replace, isInfixOf, toLower)
import qualified Data.Text as T          (map, takeWhile, null)
import qualified Data.Text.Encoding as T
import           Data.Tree               (Tree(..))
import qualified Data.Vector as V
import           PostgREST.Types
import           Text.InterpolatedString.Perl6 (qc)
import qualified Data.ByteString.Char8   as BS
import           Data.Scientific         ( FPFormat (..)
                                         , formatScientific
                                         , isInteger
                                         )
import           Protolude hiding        (from, intercalate, ord, cast)
import           PostgREST.ApiRequest    (PreferRepresentation (..))

{-| The generic query result format used by API responses. The location header
    is represented as a list of strings containing variable bindings like
    @"k1=eq.42"@, or the empty list if there is no location header.
-}
type ResultsWithCount = (Maybe Int64, Int64, [BS.ByteString], BS.ByteString)

standardRow :: HD.Row ResultsWithCount
standardRow = (,,,) <$> HD.nullableValue HD.int8 <*> HD.value HD.int8
                    <*> HD.value header <*> HD.value HD.bytea
  where
    header = HD.array $ HD.arrayDimension replicateM $ HD.arrayValue HD.bytea

noLocationF :: Text
noLocationF = "array[]::text[]"

{-| Read and Write api requests use a similar response format which includes
    various record counts and possible location header. This is the decoder
    for that common type of query.
-}
decodeStandard :: HD.Result ResultsWithCount
decodeStandard =
  HD.singleRow standardRow

decodeStandardMay :: HD.Result (Maybe ResultsWithCount)
decodeStandardMay =
  HD.maybeRow standardRow

{-| JSON and CSV payloads from the client are given to us as
    PayloadJSON (objects who all have the same keys),
    and we turn this into an old fasioned JSON array
-}
encodeUniformObjs :: HE.Params PayloadJSON
encodeUniformObjs =
  contramap (JSON.Array . V.map JSON.Object . unPayloadJSON) (HE.value HE.json)

createReadStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool -> Maybe FieldName ->
                       H.Query () ResultsWithCount
createReadStatement selectQuery countQuery isSingle countTotal asCsv binaryField =
  unicodeStatement sql HE.unit decodeStandard False
 where
  sql = [qc|
      WITH {sourceCTEName} AS ({selectQuery}) SELECT {cols}
      FROM ( SELECT * FROM {sourceCTEName}) _postgrest_t |]
  countResultF = if countTotal then "("<>countQuery<>")" else "null"
  cols = intercalate ", " [
      countResultF <> " AS total_result_set",
      "pg_catalog.count(_postgrest_t) AS page_total",
      noLocationF <> " AS header",
      bodyF <> " AS body"
    ]
  bodyF
    | asCsv = asCsvF
    | isSingle = asJsonSingleF
    | isJust binaryField = asBinaryF $ fromJust binaryField
    | otherwise = asJsonF

createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool ->
                        PreferRepresentation -> [Text] ->
                        H.Query PayloadJSON (Maybe ResultsWithCount)
createWriteStatement selectQuery mutateQuery wantSingle wantHdrs asCsv rep pKeys =
  unicodeStatement sql encodeUniformObjs decodeStandardMay True

 where
  sql = case rep of
    None -> [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT '', 0, {noLocationF}, '' |]
    HeadersOnly -> [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT {cols}
      FROM (SELECT 1 FROM {sourceCTEName}) _postgrest_t |]
    Full -> [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT {cols}
      FROM ({selectQuery}) _postgrest_t |]

  cols = intercalate ", " [
      "'' AS total_result_set", -- when updateing it does not make sense
      "pg_catalog.count(_postgrest_t) AS page_total",
      if wantHdrs
         then locationF pKeys
         else noLocationF <> " AS header",
      if rep == Full
         then bodyF <> " AS body"
         else "''"
    ]

  bodyF
    | asCsv = asCsvF
    | wantSingle = asJsonSingleF
    | otherwise = asJsonF

type ProcResults = (Maybe Int64, Int64, ByteString)
callProc :: QualifiedIdentifier -> JSON.Object -> Bool -> SqlQuery -> SqlQuery -> NonnegRange ->
  Bool -> Bool -> Bool -> Bool -> Bool -> Maybe FieldName -> H.Query () (Maybe ProcResults)
callProc qi params returnsScalar selectQuery countQuery _ countTotal isSingle paramsAsJson asCsv asBinary binaryField =
  unicodeStatement sql HE.unit decodeProc True
  where
    sql =
     if returnsScalar then [qc|
       WITH {sourceCTEName} AS ({_callSql})
       SELECT
         {countResultF} AS total_result_set,
         1 AS page_total,
         {scalarBodyF} as body
       FROM ({selectQuery}) _postgrest_t;|]
     else [qc|
       WITH {sourceCTEName} AS ({_callSql})
       SELECT
         {countResultF} AS total_result_set,
         pg_catalog.count(_postgrest_t) AS page_total,
         {bodyF} as body
       FROM ({selectQuery}) _postgrest_t;|]

    countResultF = if countTotal then "( "<> countQuery <> ")" else "null::bigint" :: Text
    _args = if paramsAsJson
                then insertableValueWithType "json" $ JSON.Object params
                else intercalate "," $ map _assignment (HM.toList params)
    _procName = qiName qi
    _assignment (n,v) = pgFmtIdent n <> ":=" <> insertableValue v
    _callSql = [qc|select * from {fromQi qi}({_args}) |] :: Text
    decodeProc = HD.maybeRow procRow
    procRow = (,,) <$> HD.nullableValue HD.int8 <*> HD.value HD.int8
                   <*> HD.value HD.bytea
    scalarBodyF
     | asBinary = asBinaryF _procName
     | otherwise = "(row_to_json(_postgrest_t)->" <> pgFmtLit _procName <> ")::character varying"

    bodyF
     | isSingle = asJsonSingleF
     | asCsv = asCsvF
     | isJust binaryField = asBinaryF $ fromJust binaryField
     | otherwise = asJsonF

pgFmtIdent :: SqlFragment -> SqlFragment
pgFmtIdent x = "\"" <> replace "\"" "\"\"" (trimNullChars $ toS x) <> "\""

pgFmtLit :: SqlFragment -> SqlFragment
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> replace "'" "''" trimmed <> "'"
     slashed = replace "\\" "\\\\" escaped in
 if "\\" `isInfixOf` escaped
   then "E" <> slashed
   else slashed

requestToCountQuery :: Schema -> DbRequest -> SqlQuery
requestToCountQuery _ (DbMutate _) = undefined
requestToCountQuery schema (DbRead (Node (Select _ _ logicForest _ _, (mainTbl, _, _, _)) _)) =
 unwords [
   "SELECT pg_catalog.count(*)",
   "FROM ", fromQi qi,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) filteredLogic)) `emptyOnFalse` null filteredLogic
   ]
 where
   qi = removeSourceCTESchema schema mainTbl
   -- all foreing key filters are root nodes(see addFilterToLogicForest), only those are filtered
   nonFKRoot :: LogicTree -> Bool
   nonFKRoot (Stmnt (Filter _ Operation{expr=(_, VForeignKey _ _)})) = False
   nonFKRoot (Stmnt _) = True
   nonFKRoot Expr{} = True
   filteredLogic = filter nonFKRoot logicForest

requestToQuery :: Schema -> Bool -> DbRequest -> SqlQuery
requestToQuery schema isParent (DbRead (Node (Select colSelects tbls logicForest ord range, (nodeName, maybeRelation, _, _)) forest)) =
  query
  where
    mainTbl = fromMaybe nodeName (tableName . relTable <$> maybeRelation)
    qi = removeSourceCTESchema schema mainTbl
    toQi = removeSourceCTESchema schema
    query = unwords [
      "SELECT ", intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
      "FROM ", intercalate ", " (map (fromQi . toQi) tbls),
      unwords joins,
      ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
      orderF (fromMaybe [] ord),
      if isParent then "" else limitF range
      ]
    orderF ts =
        if null ts
            then ""
            else "ORDER BY " <> clause
        where
            clause = intercalate "," (map queryTerm ts)
            queryTerm :: OrderTerm -> Text
            queryTerm t = " "
                <> toS (pgFmtField qi $ otTerm t) <> " "
                <> maybe "" show (otDirection t) <> " "
                <> maybe "" show (otNullOrder t) <> " "
    (joins, selects) = foldr getQueryParts ([],[]) forest

    getQueryParts :: Tree ReadNode -> ([SqlFragment], [SqlFragment]) -> ([SqlFragment], [SqlFragment])
    getQueryParts (Node n@(_, (name, Just Relation{relType=Child,relTable=Table{tableName=table}}, alias, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE(("
           <> "SELECT array_to_json(array_agg(row_to_json("<>pgFmtIdent table<>"))) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema False (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just r@Relation{relType=Parent,relTable=Table{tableName=table}}, alias, _)) forst) (j,s) = (joi:j,sel:s)
      where
        node_name = fromMaybe name alias
        local_table_name = table <> "_" <> node_name
        replaceTableName localTableName (Filter a (Operation b (c, VForeignKey (QualifiedIdentifier "" _) d))) = Filter a (Operation b (c, VForeignKey (QualifiedIdentifier "" localTableName) d))
        replaceTableName _ x = x
        sel = "row_to_json(" <> pgFmtIdent local_table_name <> ".*) AS " <> pgFmtIdent node_name
        joi = " LEFT OUTER JOIN ( " <> subquery <> " ) AS " <> pgFmtIdent local_table_name  <>
              " ON " <> intercalate " AND " ( map (pgFmtFilter qi . replaceTableName local_table_name) (getJoinFilters r) )
          where subquery = requestToQuery schema True (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just Relation{relType=Many,relTable=Table{tableName=table}}, alias, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT array_to_json(array_agg(row_to_json("<>pgFmtIdent table<>"))) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema False (DbRead (Node n forst))
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts _ _ = undefined
requestToQuery schema _ (DbMutate (Insert mainTbl (PayloadJSON rows) returnings)) =
  insInto <> vals <> ret
  where qi = QualifiedIdentifier schema mainTbl
        cols = map pgFmtIdent $ fromMaybe [] (HM.keys <$> (rows V.!? 0))
        colsString = intercalate ", " cols
        insInto = unwords [ "INSERT INTO" , fromQi qi,
            if T.null colsString then "" else "(" <> colsString <> ")"
          ]
        vals = unwords $
          if T.null colsString
            then if V.null rows then ["SELECT null WHERE false"] else ["DEFAULT VALUES"]
            else ["SELECT", colsString, "FROM json_populate_recordset(null::" , fromQi qi, ", $1)"]
        ret = if null returnings
                  then ""
                  else unwords [" RETURNING ", intercalate ", " (map (pgFmtColumn qi) returnings)]
requestToQuery schema _ (DbMutate (Update mainTbl (PayloadJSON rows) logicForest returnings)) =
  case rows V.!? 0 of
    Just obj ->
      let assignments = map
            (\(k,v) -> pgFmtIdent k <> "=" <> insertableValue v) $ HM.toList obj in
      unwords [
        "UPDATE ", fromQi qi,
        " SET " <> intercalate "," assignments <> " ",
        ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
        ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings
        ]
    Nothing -> undefined
  where
    qi = QualifiedIdentifier schema mainTbl
requestToQuery schema _ (DbMutate (Delete mainTbl logicForest returnings)) =
  query
  where
    qi = QualifiedIdentifier schema mainTbl
    query = unwords [
      "DELETE FROM ", fromQi qi,
      ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
      ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings
      ]

sourceCTEName :: SqlFragment
sourceCTEName = "pg_source"

removeSourceCTESchema :: Schema -> TableName -> QualifiedIdentifier
removeSourceCTESchema schema tbl = QualifiedIdentifier (if tbl == sourceCTEName then "" else schema) tbl

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v

-- private functions
asCsvF :: SqlFragment
asCsvF = asCsvHeaderF <> " || '\n' || " <> asCsvBodyF
  where
    asCsvHeaderF =
      "(SELECT coalesce(string_agg(a.k, ','), '')" <>
      "  FROM (" <>
      "    SELECT json_object_keys(r)::TEXT as k" <>
      "    FROM ( " <>
      "      SELECT row_to_json(hh) as r from " <> sourceCTEName <> " as hh limit 1" <>
      "    ) s" <>
      "  ) a" <>
      ")"
    asCsvBodyF = "coalesce(string_agg(substring(_postgrest_t::text, 2, length(_postgrest_t::text) - 2), '\n'), '')"

asJsonF :: SqlFragment
asJsonF = "coalesce(array_to_json(array_agg(row_to_json(_postgrest_t))), '[]')::character varying"

asJsonSingleF :: SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "coalesce(string_agg(row_to_json(_postgrest_t)::text, ','), '')::character varying "

asBinaryF :: FieldName -> SqlFragment
asBinaryF fieldName = "coalesce(string_agg(_postgrest_t." <> pgFmtIdent fieldName <> ", ''), '')"

locationF :: [Text] -> SqlFragment
locationF pKeys =
    "(" <>
    " WITH s AS (SELECT row_to_json(ss) as r from " <> sourceCTEName <> " as ss  limit 1)" <>
    " SELECT array_agg(json_data.key || '=' || coalesce('eq.' || json_data.value, 'is.null'))" <>
    " FROM s, json_each_text(s.r) AS json_data" <>
    (
      if null pKeys
      then ""
      else " WHERE json_data.key IN ('" <> intercalate "','" pKeys <> "')"
    ) <> ")"

limitF :: NonnegRange -> SqlFragment
limitF r  = if r == allRange
  then ""
  else "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" show $ rangeLimit r
    offset = show $ rangeOffset r

fromQi :: QualifiedIdentifier -> SqlFragment
fromQi t = (if s == "" then "" else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

getJoinFilters :: Relation -> [Filter]
getJoinFilters (Relation t cols ft fcs typ lt lc1 lc2) =
  case typ of
    Child  -> zipWith (toFilter tN ftN) cols fcs
    Parent -> zipWith (toFilter tN ftN) cols fcs
    Many   -> zipWith (toFilter tN ltN) cols (fromMaybe [] lc1) ++ zipWith (toFilter ftN ltN) fcs (fromMaybe [] lc2)
    Root   -> undefined --error "undefined getJoinFilters"
  where
    s = if typ == Parent then "" else tableSchema t
    tN = tableName t
    ftN = tableName ft
    ltN = fromMaybe "" (tableName <$> lt)
    toFilter :: Text -> Text -> Column -> Column -> Filter
    toFilter tb ftb c fc = Filter (colName c, Nothing) (Operation False ("=", VForeignKey (QualifiedIdentifier s tb) (ForeignKey fc{colTable=(colTable fc){tableName=ftb}})))

unicodeStatement :: Text -> HE.Params a -> HD.Result b -> Bool -> H.Query a b
unicodeStatement = H.statement . T.encodeUtf8

emptyOnFalse :: Text -> Bool -> Text
emptyOnFalse val cond = if cond then "" else val

insertableValue :: JSON.Value -> SqlFragment
insertableValue JSON.Null = "null"
insertableValue v = (<> "::unknown") . pgFmtLit $ unquoted v

insertableValueWithType :: Text -> JSON.Value -> SqlFragment
insertableValueWithType t v =
  pgFmtLit (unquoted v) <> "::" <> t

pgFmtColumn :: QualifiedIdentifier -> Text -> SqlFragment
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtField :: QualifiedIdentifier -> Field -> SqlFragment
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> SqlFragment
pgFmtSelectItem table (f@(_, jp), Nothing, alias, _) = pgFmtField table f <> pgFmtAs jp alias
pgFmtSelectItem table (f@(_, jp), Just cast, alias, _) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAs jp alias

pgFmtFilter :: QualifiedIdentifier -> Filter -> SqlFragment
pgFmtFilter table (Filter fld (Operation hasNot_ ex)) = notOp <> " " <> case ex of
   (op, VText val) -> pgFmtFieldOp op <> " " <> case op of
     "like"  -> unknownLiteral (T.map star val)
     "ilike" -> unknownLiteral (T.map star val)
     "@@"    -> "to_tsquery(" <> unknownLiteral val <> ") "
     "is"    -> whiteList val
     "isnot" -> whiteList val
     _       -> unknownLiteral val
   (op, VTextL vals) -> pgFmtIn op vals -- in and notin
   (op, VForeignKey fQi (ForeignKey Column{colTable=Table{tableName=fTableName}, colName=fColName})) ->
     pgFmtField fQi fld <> " " <> sqlOperator op <> " " <> pgFmtColumn (removeSourceCTESchema (qiSchema fQi) fTableName) fColName
 where
   pgFmtFieldOp op = pgFmtField table fld <> " " <> sqlOperator op
   sqlOperator o = HM.lookupDefault "=" o operators
   notOp = if hasNot_ then "NOT" else ""
   star c = if c == '*' then '%' else c
   unknownLiteral = (<> "::unknown ") . pgFmtLit
   whiteList :: Text -> SqlFragment
   whiteList v = fromMaybe
     (toS (pgFmtLit v) <> "::unknown ")
     (find ((==) . toLower $ v) ["null","true","false"])
   pgFmtIn :: Operator -> [Text] -> SqlFragment
   pgFmtIn op vals =
    -- Workaround because for postgresql "col IN ()" is invalid syntax, we instead do "col = any('{}')"
    let emptyValForIn o = (if "not" `isInfixOf` o then "NOT " else "") -- handle case of "notin" operator
                          <> pgFmtField table fld <> " = any('{}') " in
    case T.null <$> headMay vals of
      Just isNull -> if isNull && length vals == 1
                        then emptyValForIn op
                        else pgFmtFieldOp op <> "(" <> intercalate ", " (map unknownLiteral vals) <> ") "
      Nothing -> emptyValForIn op

pgFmtLogicTree :: QualifiedIdentifier -> LogicTree -> SqlFragment
pgFmtLogicTree qi (Expr hasNot_ op forest) = notOp <> " (" <> intercalate (" " <> show op <> " ") (pgFmtLogicTree qi <$> forest) <> ")"
  where notOp =  if hasNot_ then "NOT" else ""
pgFmtLogicTree qi (Stmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: Maybe JsonPath -> SqlFragment
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtAs :: Maybe JsonPath -> Maybe Alias -> SqlFragment
pgFmtAs Nothing Nothing = ""
pgFmtAs (Just xx) Nothing = case lastMay xx of
  Just alias -> " AS " <> pgFmtIdent alias
  Nothing -> ""
pgFmtAs _ (Just alias) = " AS " <> pgFmtIdent alias

pgFmtEnvVar :: Text -> (Text, Text) -> SqlFragment
pgFmtEnvVar prefix (k, v) =
  "set local " <> pgFmtIdent (prefix <> k) <> " = " <> pgFmtLit v <> ";"

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')
