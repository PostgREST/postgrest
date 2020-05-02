{-|
Module      : PostgREST.DbStructure
Description : PostgREST schema cache

This module contains queries that target PostgreSQL system catalogs, these are used to build the schema cache(DbStructure).

The schema cache is necessary for resource embedding, foreign keys are used for inferring the relationships between tables.

These queries are executed once at startup or when PostgREST is reloaded.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module PostgREST.DbStructure (
  getDbStructure
, accessibleTables
, accessibleProcs
, schemaDescription
, getPgVersion
) where

import           Control.Exception
import qualified Data.Aeson                    as Aeson
import qualified Data.FileEmbed                as FileEmbed
import qualified Data.HashMap.Strict           as M
import qualified Data.List                     as L
import           Data.Set                      as S (fromList)
import           Data.String
import           Data.Text                     (breakOn, dropAround,
                                                split, splitOn, strip)
import qualified Data.Text                     as T
import           GHC.Exts                      (groupWith)
import qualified Hasql.Decoders                as HD
import qualified Hasql.Encoders                as HE
import qualified Hasql.Session                 as H
import qualified Hasql.Statement               as H
import qualified Hasql.Transaction             as HT
import           PostgREST.Private.Common
import           PostgREST.Types
import           Protolude                     hiding (toS)
import           Protolude.Conv                (toS)
import           Protolude.Unsafe              (unsafeHead)
import           Text.InterpolatedString.Perl6 (q)

getDbStructure :: [Schema] -> PgVersion -> HT.Transaction DbStructure
getDbStructure schemas pgVer = do
  HT.sql "set local schema ''" -- This voids the search path. The following queries need this for getting the fully qualified name(schema.name) of every db object
  raw <- getRawDbStructure schemas

  let
    tabs = rawDbTables raw
    cols = rawDbColumns raw
    srcCols = rawDbSourceColumns raw
    oldSrcCols = fmap (\src -> (srcSource src, srcView src)) srcCols
    keys = rawDbPrimaryKeys raw
    rawProcs = rawDbProcs raw
    procs = procsMap $ fmap loadProc rawProcs
    m2oRels = rawDbM2oRels raw

  let rels = addM2MRels . addO2MRels $ addViewM2ORels oldSrcCols m2oRels
      cols' = addForeignKeys rels cols
      keys' = addViewPrimaryKeys oldSrcCols keys

  return DbStructure {
      dbTables = tabs
    , dbColumns = cols'
    , dbRelations = rels
    , dbPrimaryKeys = keys'
    , dbProcs = procs
    , pgVersion = pgVer
    }


procsMap :: [ProcDescription] -> ProcsMap
procsMap procs =
    M.fromListWith (++) . map (\(x,y) -> (x, [y])) . sort $ map addKey procs

loadProc :: RawProcDescription -> ProcDescription
loadProc raw =
  ProcDescription
    { pdSchema = procSchema raw
    , pdName = procName raw
    , pdDescription = procDescription raw
    , pdArgs = parseArgs $ procArgs raw
    , pdReturnType =
        parseRetType
          (procReturnTypeSchema raw)
          (procReturnTypeName raw)
          (procReturnTypeIsSetof raw)
          (procReturnType raw)
    , pdVolatility =
        parseVolatility (procVolatility raw)
    }

addKey :: ProcDescription -> (QualifiedIdentifier, ProcDescription)
addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

parseArgs :: Text -> [PgArg]
parseArgs =
    mapMaybe parseArg . filter (not . isPrefixOf "OUT" . toS) . map strip . split (==',')

parseArg :: Text -> Maybe PgArg
parseArg a =
  let arg = lastDef "" $ splitOn "INOUT " a
      (body, def) = breakOn " DEFAULT " arg
      (name, typ) = breakOn " " body in
  if T.null typ
     then Nothing
     else Just $
       PgArg (dropAround (== '"') name) (strip typ) (T.null def)

parseRetType :: Text -> Text -> Bool -> Char -> RetType
parseRetType schema name isSetOf typ
  | isSetOf   = SetOf pgType
  | otherwise = Single pgType
  where
    qi = QualifiedIdentifier schema name
    pgType = case typ of
      'c' -> Composite qi
      'p' -> if name == "record" -- Only pg pseudo type that is a row type is 'record'
               then Composite qi
               else Scalar qi
      _   -> Scalar qi -- 'b'ase, 'd'omain, 'e'num, 'r'ange

parseVolatility :: Char -> ProcVolatility
parseVolatility v | v == 'i' = Immutable
                  | v == 's' = Stable
                  | otherwise = Volatile -- only 'v' can happen here

decodeTables :: HD.Result [Table]
decodeTables =
  HD.rowList tblRow
 where
  tblRow = Table <$> column HD.text
                 <*> column HD.text
                 <*> nullableColumn HD.text
                 <*> column HD.bool

decodeProcs :: HD.Result ProcsMap
decodeProcs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to ProcDescription Ord instance
  map sort . M.fromListWith (++) . map ((\(x,y) -> (x, [y])) . addKey) <$> HD.rowList procRow
  where
     procRow = ProcDescription
        <$> column HD.text
        <*> column HD.text
        <*> nullableColumn HD.text
        <*> (parseArgs <$> column HD.text)
        <*> (parseRetType
        <$> column HD.text
        <*> column HD.text
        <*> column HD.bool
        <*> column HD.char)
        <*> (parseVolatility <$> column HD.char)

accessibleProcs :: H.Statement Schema ProcsMap
accessibleProcs = H.Statement (toS sql) (param HE.text) decodeProcs True
  where
    sql = procsSqlQuery <> " WHERE pn.nspname = $1 AND has_function_privilege(p.oid, 'execute')"

procsSqlQuery :: SqlQuery
procsSqlQuery = [q|
  SELECT
    pn.nspname as proc_schema,
    p.proname as proc_name,
    d.description as proc_description,
    pg_get_function_arguments(p.oid) as args,
    tn.nspname as rettype_schema,
    coalesce(comp.relname, t.typname) as rettype_name,
    p.proretset as rettype_is_setof,
    t.typtype as rettype_typ,
    p.provolatile
  FROM pg_proc p
    JOIN pg_namespace pn ON pn.oid = p.pronamespace
    JOIN pg_type t ON t.oid = p.prorettype
    JOIN pg_namespace tn ON tn.oid = t.typnamespace
    LEFT JOIN pg_class comp ON comp.oid = t.typrelid
    LEFT JOIN pg_catalog.pg_description as d on d.objoid = p.oid
|]

schemaDescription :: H.Statement Schema (Maybe Text)
schemaDescription =
    H.Statement sql (param HE.text) (join <$> HD.rowMaybe (nullableColumn HD.text)) True
  where
    sql = [q|
      select
        description
      from
        pg_catalog.pg_namespace n
        left join pg_catalog.pg_description d on d.objoid = n.oid
      where
        n.nspname = $1 |]

accessibleTables :: H.Statement Schema [Table]
accessibleTables =
  H.Statement sql (param HE.text) decodeTables True
 where
  sql = [q|
    select
      n.nspname as table_schema,
      relname as table_name,
      d.description as table_description,
      (
        c.relkind in ('r', 'v', 'f')
        and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
        -- The function `pg_relation_is_updateable` returns a bitmask where 8
        -- corresponds to `1 << CMD_INSERT` in the PostgreSQL source code, i.e.
        -- it's possible to insert into the relation.
        or (exists (
          select 1
          from pg_trigger
          where
            pg_trigger.tgrelid = c.oid
            and (pg_trigger.tgtype::integer & 69) = 69)
            -- The trigger type `tgtype` is a bitmask where 69 corresponds to
            -- TRIGGER_TYPE_ROW + TRIGGER_TYPE_INSTEAD + TRIGGER_TYPE_INSERT
            -- in the PostgreSQL source code.
        )
      ) as insertable
    from
      pg_class c
      join pg_namespace n on n.oid = c.relnamespace
      left join pg_catalog.pg_description as d on d.objoid = c.oid and d.objsubid = 0
    where
      c.relkind in ('v', 'r', 'm', 'f')
      and n.nspname = $1
      and (
        pg_has_role(c.relowner, 'USAGE')
        or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
        or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
      )
    order by relname |]

addForeignKeys :: [Relation] -> [Column] -> [Column]
addForeignKeys rels = map addFk
  where
    addFk col = col { colFK = fk col }
    fk col = find (lookupFn col) rels >>= relToFk col
    lookupFn :: Column -> Relation -> Bool
    lookupFn c Relation{relColumns=cs, relType=rty} = c `elem` cs && rty==M2O
    relToFk col Relation{relColumns=cols, relFColumns=colsF} = do
      pos <- L.elemIndex col cols
      colF <- atMay colsF pos
      return $ ForeignKey colF

{-
Adds Views M2O Relations based on SourceColumns found, the logic is as follows:

Having a Relation{relTable=t1, relColumns=[c1], relFTable=t2, relFColumns=[c2], relType=M2O} represented by:

t1.c1------t2.c2

When only having a t1_view.c1 source column, we need to add a View-Table M2O Relation

         t1.c1----t2.c2         t1.c1----------t2.c2
                         ->            ________/
                                      /
      t1_view.c1             t1_view.c1


When only having a t2_view.c2 source column, we need to add a Table-View M2O Relation

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________
                                                    \
                    t2_view.c2                      t2_view.c1

When having t1_view.c1 and a t2_view.c2 source columns, we need to add a View-View M2O Relation in addition to the prior

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________/
                                           /        \
    t1_view.c1     t2_view.c2     t1_view.c1-------t2_view.c1

The logic for composite pks is similar just need to make sure all the Relation columns have source columns.
-}
addViewM2ORels :: [OldSourceColumn] -> [Relation] -> [Relation]
addViewM2ORels allSrcCols = concatMap (\rel ->
  rel : case rel of
    Relation{relType=M2O, relTable, relColumns, relConstraint, relFTable, relFColumns} ->

      let srcColsGroupedByView :: [Column] -> [[OldSourceColumn]]
          srcColsGroupedByView relCols = L.groupBy (\(_, viewCol1) (_, viewCol2) -> colTable viewCol1 == colTable viewCol2) $
                                         filter (\(c, _) -> c `elem` relCols) allSrcCols
          relSrcCols = srcColsGroupedByView relColumns
          relFSrcCols = srcColsGroupedByView relFColumns
          getView :: [OldSourceColumn] -> Table
          getView = colTable . snd . unsafeHead
          srcCols `allSrcColsOf` cols = S.fromList (fst <$> srcCols) == S.fromList cols
          -- Relation is dependent on the order of relColumns and relFColumns to get the join conditions right in the generated query.
          -- So we need to change the order of the SourceColumns to match the relColumns
          -- TODO: This could be avoided if the Relation type is improved with a structure that maintains the association of relColumns and relFColumns
          srcCols `sortAccordingTo` cols = sortOn (\(k, _) -> L.lookup k $ zip cols [0::Int ..]) srcCols

          viewTableM2O =
            [ Relation (getView srcCols) (snd <$> srcCols `sortAccordingTo` relColumns)
                       relConstraint relFTable relFColumns
                       M2O Nothing
            | srcCols <- relSrcCols, srcCols `allSrcColsOf` relColumns ]

          tableViewM2O =
            [ Relation relTable relColumns
                       relConstraint
                       (getView fSrcCols) (snd <$> fSrcCols `sortAccordingTo` relFColumns)
                       M2O Nothing
            | fSrcCols <- relFSrcCols, fSrcCols `allSrcColsOf` relFColumns ]

          viewViewM2O =
            [ Relation (getView srcCols) (snd <$> srcCols `sortAccordingTo` relColumns)
                       relConstraint
                       (getView fSrcCols) (snd <$> fSrcCols `sortAccordingTo` relFColumns)
                       M2O Nothing
            | srcCols  <- relSrcCols, srcCols `allSrcColsOf` relColumns
            , fSrcCols <- relFSrcCols, fSrcCols `allSrcColsOf` relFColumns ]

      in viewTableM2O ++ tableViewM2O ++ viewViewM2O

    _ -> [])

addO2MRels :: [Relation] -> [Relation]
addO2MRels = concatMap (\rel@(Relation t c cn ft fc _ _) -> [rel, Relation ft fc cn t c O2M Nothing])

addM2MRels :: [Relation] -> [Relation]
addM2MRels rels = rels ++ addMirrorRel (mapMaybe junction2Rel junctions)
  where
    junctions = join $ map (combinations 2) $ filter (not . null) $ groupWith groupFn $ filter ( (==M2O). relType) rels
    groupFn :: Relation -> Text
    groupFn Relation{relTable=Table{tableSchema=s, tableName=t}} = s <> "_" <> t
    -- Reference : https://wiki.haskell.org/99_questions/Solutions/26
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _  = [ [] ]
    combinations n xs = [ y:ys | y:xs' <- tails xs
                               , ys <- combinations (n-1) xs']
    junction2Rel [
      Relation{relTable=jt, relColumns=jc1, relConstraint=const1, relFTable=t,  relFColumns=c},
      Relation{             relColumns=jc2, relConstraint=const2, relFTable=ft, relFColumns=fc}
      ]
      | jc1 /= jc2 && length jc1 == 1 && length jc2 == 1 = Just $ Relation t c Nothing ft fc M2M (Just $ Junction jt const1 jc1 const2 jc2)
      | otherwise = Nothing
    junction2Rel _ = Nothing
    addMirrorRel = concatMap (\rel@(Relation t c _ ft fc _ (Just (Junction jt const1 jc1 const2 jc2))) ->
      [rel, Relation ft fc Nothing t c M2M (Just (Junction jt const2 jc2 const1 jc1))])

addViewPrimaryKeys :: [OldSourceColumn] -> [PrimaryKey] -> [PrimaryKey]
addViewPrimaryKeys srcCols = concatMap (\pk ->
  let viewPks = (\(_, viewCol) -> PrimaryKey{pkTable=colTable viewCol, pkName=colName viewCol}) <$>
                filter (\(col, _) -> colTable col == pkTable pk && colName col == pkName pk) srcCols in
  pk : viewPks)

getPgVersion :: H.Session PgVersion
getPgVersion = H.statement () $ H.Statement sql HE.noParams versionRow False
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version')"
    versionRow = HD.singleRow $ PgVersion <$> column HD.int4 <*> column HD.text



-- RAW DB STRUCTURE


getRawDbStructure :: [Schema] -> HT.Transaction RawDbStructure
getRawDbStructure schemas =
    do
        value <- HT.statement schemas rawDbStructureQuery

        case Aeson.fromJSON value of
          Aeson.Success m ->
              return m
          Aeson.Error err ->
              throw $ DbStructureDecodeException err

data DbStructureDecodeException =
    DbStructureDecodeException String
    deriving Show

instance Exception DbStructureDecodeException

rawDbStructureQuery :: H.Statement [Schema] Aeson.Value
rawDbStructureQuery =
  let
    sql =
      $(FileEmbed.embedFile "dbstructure/query.sql")

    decode =
      HD.singleRow $ column HD.json
  in
  H.Statement sql (arrayParam HE.text) decode True
