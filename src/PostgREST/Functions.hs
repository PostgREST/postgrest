{-# LANGUAGE OverloadedStrings #-}
module PostgREST.Functions
where

import PostgREST.Types
import           Control.Error
import Data.List (find)
import Data.Tree
import Data.Text hiding (find, foldr, map, null, last, head)
import Data.Monoid
import PostgREST.PgQuery (pgFmtOperator, pgFmtValue, pgFmtIdent, pgFmtLit, fromQi, whiteList, QualifiedIdentifier(..))


findColumn :: [Column] -> Text -> Text -> Text -> Either Text Column
findColumn allColumns s t c = note ("no such column: "<>t<>"."<>c) $
    find (\ col -> colSchema col == s && colTable col == t && colName col == c ) allColumns

findTable :: [Table] -> Text -> Text -> Either Text Table
findTable allTables s t = note ("no such table: "<>t) $
    find (\tb-> s == tableSchema tb && t == tableName tb ) allTables

findRelation :: [Relation] -> Text -> Text -> Text -> Maybe Relation
findRelation allRelations s t1 t2 =
    find (\r -> s == relSchema r && t1 == relTable r && t2 == relFTable r) allRelations


filterToCondition :: Text -> [Column] -> Text -> Filter -> Either Text Condition
filterToCondition schema allColumns table (Filter fld op val) =
    Condition <$> c <*> pure op <*> pure (VText (pack val))
    where
        c = (,) <$> column <*> pure (snd fld)
        column = findColumn allColumns schema table $ pack $ fst fld


requestNodeToQuery ::Text -> [Table] -> [Column] -> RequestNode -> Either Text Query
requestNodeToQuery schema allTables allColumns (RequestNode tblNameS flds fltrs) =
    Select <$> mainTable <*> select <*> joinTables <*> qwhere <*> rel
    where
        tblName = pack tblNameS
        mainTable = findTable allTables schema tblName
        select = mapM toDbSelectItem flds --besides specific columns, we allow * here also
            where
                -- it's ok not to check that the table exists here, mainTable will do the checking
                toDbSelectItem :: SelectItem -> Either Text DbSelectItem
                toDbSelectItem (("*", Nothing), Nothing) = Right $ ((Star{colSchema = schema, colTable = tblName}, Nothing), Nothing)
                toDbSelectItem ((c,jp), cast) = (,) <$> dbFld <*> pure cast
                    where
                        col = findColumn allColumns schema tblName $ pack c
                        dbFld = (,) <$> col <*> pure jp

        qwhere = mapM (filterToCondition schema allColumns tblName) fltrs
        joinTables = pure []
        rel = pure Nothing

addRelations :: [Relation] -> Maybe DbRequest -> DbRequest -> Either Text DbRequest
addRelations allRelations parentNode node@(Node query@(Select {qMainTable=table}) forest) =
    case parentNode of
        Nothing -> Node query{qRelation=Nothing} <$> updatedForest
        (Just (Node (Select{qMainTable=parentTable}) _)) -> Node <$> (addRel query <$> rel) <*> updatedForest
            where
                rel = note ("no relation between " <> (tableName table) <> " and " <> (tableName parentTable)) $
                    findRelation allRelations (tableSchema table) (tableName table) (tableName parentTable)
                addRel :: Query -> Relation -> Query
                addRel q r = q{qRelation = Just r}
    where
        updatedForest = mapM (addRelations allRelations (Just node)) forest


addJoinConditions :: [Column] -> Tree Query -> Either Text DbRequest
addJoinConditions allColumns (Node query@(Select{qRelation=relation}) forest) =
    case relation of
        Nothing -> Node <$> updatedQuery <*> updatedForest -- this is the root node
        Just rel@(Relation{relType="child"}) -> Node <$> (addCond <$> updatedQuery <*> getJoinCondition rel) <*> updatedForest
        Just (Relation{relType="parent"}) -> Node <$> updatedQuery <*> updatedForest
        -- Just (Many relationColumn1 relationColumn2) -> Node <$> pure updatedQuery{qJoinTables=linkTable:qJoinTables updatedQuery, qWhere=cond1:cond2:qWhere updatedQuery} <*> updatedForest
            -- where
            --     cond1 = getJoinCondition relationColumn1
            --     cond2 = getJoinCondition relationColumn2
            --     linkTable = Table "public" (colTable relationColumn1) True
        _ -> Left "unknow relation"
    where
        -- add parentTable and parentJoinConditions to the query
        updatedQuery = foldr (flip addCond) (query{qJoinTables = parentTables ++ (qJoinTables query)}) <$> parentJoinConditions
            where
                parentJoinConditions = mapM (getJoinCondition.snd) parents
                parentTables = map fst parents
                parents = mapMaybe (getParents.rootLabel) forest
                getParents qq@(Select{qRelation=(Just rel@(Relation{relType="parent"}))}) = Just (qMainTable qq, rel)
                getParents _ = Nothing
        updatedForest = mapM (addJoinConditions allColumns) forest
        getJoinCondition rel@(Relation s t c _ _ _) = Condition <$> cc <*> pure "=" <*> pure (VForeignKey rel)
            where
                col = findColumn allColumns s t c
                cc = (,) <$> col <*> pure Nothing
        addCond q con = q{qWhere=con:qWhere q}


dbRequestToCountQuery :: DbRequest -> Text
dbRequestToCountQuery (Node (Select mainTable columns tables conditions relation) forest) =
    Data.Text.unwords [
        "SELECT pg_catalog.count(1)",
        "FROM ", pgFmtTable mainTable,
        ("WHERE " <> intercalate " AND " ( map pgFmtCondition conditions )) `emptyOnNull` conditions
      ]
    where emptyOnNull val x = if null x then "" else val

dbRequestToQuery :: DbRequest -> Text
dbRequestToQuery r@(Node (Select mainTable columns tables conditions relation) forest) =
    case relation of
        Nothing -> "SELECT "
                  <> "("
                  <> dbRequestToCountQuery r
                  <> "),"
                  <> "pg_catalog.count(t),"
                  <> "array_to_json(array_agg(row_to_json(t)))::CHARACTER VARYING AS json "
                  <> "FROM ("
                  <> query
                  <> ") t;"

        _         -> query
    where
        query = Data.Text.unwords [
            ("WITH " <> intercalate ", " withs) `emptyOnNull` withs,
            "SELECT ", intercalate ", " (map selectItemToStr columns ++ selects),
            "FROM ", intercalate ", " (map pgFmtTable (mainTable:tables)),
            ("WHERE " <> intercalate " AND " ( map pgFmtCondition conditions )) `emptyOnNull` conditions
            ]
        emptyOnNull val x = if null x then "" else val
        (withs, selects) = foldr getQueryParts ([],[]) forest
        --getQueryParts is not total but dbRequestToQuery is called only after addJoinConditions which ensures the only
        --posible relations are Child Parent Many
        getQueryParts :: Tree Query -> ([Text], [Text]) -> ([Text], [Text])
        getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Relation {relType="child"}))}) forst) (w,s) = (w,sel:s)
            where name = tableName table
                  sel = "("
                     <> "SELECT array_to_json(array_agg(row_to_json("<>name<>"))) "
                     <> "FROM (" <> dbRequestToQuery (Node q forst) <> ") " <> name
                     <> ") AS " <> name
        getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Relation{relType="parent"}))}) forst) (w,s) = (wit:w,sel:s)
            where name = tableName table
                  sel = "row_to_json(" <> name <> ".*) AS "<>name --TODO must be singular
                  wit = name <> " AS ( " <> dbRequestToQuery (Node q forst) <> " )"
        -- getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Many _ _))}) forst) (w,s) = (w,sel:s)
        --     where name = tableName table
        --           sel = "("
        --              <> "SELECT array_to_json(array_agg(row_to_json("<>name<>"))) "
        --              <> "FROM (" <> dbRequestToQuery (Node q forst) <> ") " <> name
        --              <> ") AS " <> name
        -- the following is just to remove the warning, maybe relType should not be String?
        getQueryParts (Node (Select{qRelation=Nothing}) _) _ = undefined
        getQueryParts (Node (Select{qRelation=(Just (Relation {relType=_}))}) _) _ = undefined

pgFmtCondition :: Condition -> Text
pgFmtCondition (Condition (col,jp) ops val) =
    notOp <> " " <> pgFmtColumn col <> pgFmtJsonPath jp  <> " " <> pgFmtOperator opCode <> " " <>
      if opCode `elem` ["is","isnot"] then whiteList (getInner val) else sqlValue
    where
        headPredicate:rest = split (=='.') $ pack ops
        hasNot caseTrue caseFalse = if headPredicate == "not" then caseTrue else caseFalse
        opCode        = hasNot (head rest) headPredicate
        notOp         = hasNot headPredicate ""
        sqlValue = valToStr val
        getInner v = case v of
            VText s -> s
            _       -> ""
        valToStr v = case v of
            VText s -> pgFmtValue opCode s
            VForeignKey (Relation{relFTable=table, relFColumn=column}) -> table <> "." <> column

pgFmtColumn :: Column -> Text
pgFmtColumn Column {colSchema=s, colTable=t, colName=c} = pgFmtIdent s <> "." <> pgFmtIdent t <> "." <> pgFmtIdent c
pgFmtColumn Star {colSchema=s, colTable=t} = pgFmtIdent s <> "." <> pgFmtIdent t <> ".*"

pgFmtJsonPath :: Maybe JsonPath -> Text
pgFmtJsonPath (Just [x]) = "->>" <> (pgFmtLit $ pack x)
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit (pack x) <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtTable :: Table -> Text
pgFmtTable Table{tableSchema=s, tableName=n} = fromQi $ QualifiedIdentifier s n

selectItemToStr :: DbSelectItem -> Text
selectItemToStr ((c, jp), Nothing) = pgFmtColumn c <> pgFmtJsonPath jp <> asJsonPath jp
selectItemToStr ((c, jp), Just cast ) = "CAST (" <> pgFmtColumn c <> pgFmtJsonPath jp <> " AS " <> pack cast <> " )" <> asJsonPath jp

asJsonPath :: Maybe JsonPath -> Text
asJsonPath Nothing = ""
asJsonPath (Just xx) = " AS " <> (pack $ last xx)
