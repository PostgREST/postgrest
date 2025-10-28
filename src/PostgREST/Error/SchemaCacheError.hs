{-# LANGUAGE RecordWildCards #-}
module PostgREST.Error.SchemaCacheError
  ( SchemaCacheError (..),
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.FuzzySet             as Fuzzy
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Status as HTTP

import PostgREST.MediaType                (MediaType (..))
import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..),
                                           Schema)
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.SchemaCache.Routine      (Routine (..),
                                           RoutineParam (..))
import PostgREST.SchemaCache.Table        (Table (..))
import PostgREST.Error.Algebra
import Protolude

data SchemaCacheError
  = AmbiguousRelBetween Text Text [Relationship]
  | AmbiguousRpc [Routine]
  | NoRelBetween Text Text (Maybe Text) Text RelationshipsMap
  | NoRpc Text Text [Text] MediaType Bool [QualifiedIdentifier] [Routine]
  | ColumnNotFound Text Text
  | TableNotFound Text Text [Table]
  deriving Show

instance PgrstError SchemaCacheError where
  status AmbiguousRelBetween{} = HTTP.status300
  status AmbiguousRpc{}        = HTTP.status300
  status NoRelBetween{}        = HTTP.status400
  status NoRpc{}               = HTTP.status404
  status ColumnNotFound{}      = HTTP.status400
  status TableNotFound{}       = HTTP.status404

  headers _ = mempty

instance ErrorBody SchemaCacheError where
  code NoRelBetween{}        = "PGRST200"
  code AmbiguousRelBetween{} = "PGRST201"
  code NoRpc{}               = "PGRST202"
  code AmbiguousRpc{}        = "PGRST203"
  code ColumnNotFound{}      = "PGRST204"
  code TableNotFound{}       = "PGRST205"

  message (NoRelBetween parent child _ _ _)  = "Could not find a relationship between '" <> parent <> "' and '" <> child <> "' in the schema cache"
  message (AmbiguousRelBetween parent child _) = "Could not embed because more than one relationship was found for '" <> parent <> "' and '" <> child <> "'"
  message (NoRpc schema procName argumentKeys contentType isInvPost _ _) = "Could not find the function " <> func <> (if onlySingleParams then "" else fmtPrms prmsMsg) <> " in the schema cache"
      where
        onlySingleParams = isInvPost && contentType `elem` [MTTextPlain, MTTextXML, MTOctetStream]
        func = schema <> "." <> procName
        prms = T.intercalate ", " argumentKeys
        prmsMsg = "(" <> prms <> ")"
        fmtPrms p = if null argumentKeys then " without parameters" else p
  message (AmbiguousRpc procs) = "Could not choose the best candidate function between: " <> T.intercalate ", " [pdSchema p <> "." <> pdName p <> "(" <> T.intercalate ", " [ppName a <> " => " <> ppType a | a <- pdParams p] <> ")" | p <- procs]
  message (ColumnNotFound rel col) = "Could not find the '" <> col <> "' column of '" <> rel <> "' in the schema cache"
  message (TableNotFound schemaName relName _) = "Could not find the table '" <> schemaName <> "." <> relName <> "' in the schema cache"

  details (NoRelBetween parent child embedHint schema _) = Just $ JSON.String $ "Searched for a foreign key relationship between '" <> parent <> "' and '" <> child <> maybe mempty ("' using the hint '" <>) embedHint <> "' in the schema '" <> schema <> "', but no matches were found."
  details (AmbiguousRelBetween _ _ rels)       = Just $ JSON.toJSONList (compressedRel <$> rels)
  details (NoRpc schema procName argumentKeys contentType isInvPost _ _) =
      Just $ JSON.String $ "Searched for the function " <> func <>
        (case (isInvPost, contentType) of
           (True, MTTextPlain)       -> " with a single unnamed text parameter"
           (True, MTTextXML)         -> " with a single unnamed xml parameter"
           (True, MTOctetStream)     -> " with a single unnamed bytea parameter"
           (True, MTApplicationJSON) -> fmtPrms prmsDet <> " or with a single unnamed json/jsonb parameter"
           _                         -> fmtPrms prmsDet
        ) <> ", but no matches were found in the schema cache."
      where
        func = schema <> "." <> procName
        prms = T.intercalate ", " argumentKeys
        prmsDet = " with parameter" <> (if length argumentKeys > 1 then "s " else " ") <> prms
        fmtPrms p = if null argumentKeys then " without parameters" else p

  details _ = Nothing

  hint (NoRelBetween parent child _ schema allRels) = JSON.String <$> noRelBetweenHint parent child schema allRels
  hint (AmbiguousRelBetween _ child rels)   = Just $ JSON.String $ "Try changing '" <> child <> "' to one of the following: " <> relHint rels <> ". Find the desired relationship in the 'details' key."
  -- The hint will be null in the case of single unnamed parameter functions
  hint (NoRpc schema procName argumentKeys contentType isInvPost allProcs overloadedProcs) =
    if onlySingleParams
      then Nothing
      else JSON.String <$> noRpcHint schema procName argumentKeys allProcs overloadedProcs
      where
        onlySingleParams = isInvPost && contentType `elem` [MTTextPlain, MTTextXML, MTOctetStream]
  hint (AmbiguousRpc _)      = Just "Try renaming the parameters or the function itself in the database so function overloading can be resolved"
  hint (TableNotFound schemaName relName tbls) = JSON.String <$> tableNotFoundHint schemaName relName tbls

  hint _ = Nothing

instance JSON.ToJSON SchemaCacheError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

-- |
-- Do a fuzzy search in all tables in the same schema and return closest result
tableNotFoundHint :: Text -> Text -> [Table] -> Maybe Text
tableNotFoundHint schema tblName tblList
  = fmap (\tbl -> "Perhaps you meant the table '" <> schema <> "." <> tbl <> "'") perhapsTable
    where
      perhapsTable = Fuzzy.getOne fuzzyTableSet tblName
      fuzzyTableSet = Fuzzy.fromList [ tableName tbl | tbl <- tblList, tableSchema tbl == schema]

-- |
-- If no function is found with the given name, it does a fuzzy search to all the functions
-- in the same schema and shows the best match as hint.
--
-- >>> :set -Wno-missing-fields
-- >>> let procs = [(QualifiedIdentifier "api" "test"), (QualifiedIdentifier "api" "another"), (QualifiedIdentifier "private" "other")]
--
-- >>> noRpcHint "api" "testt" ["val", "param", "name"] procs []
-- Just "Perhaps you meant to call the function api.test"
--
-- >>> noRpcHint "api" "other" [] procs []
-- Just "Perhaps you meant to call the function api.another"
--
-- >>> noRpcHint "api" "noclosealternative" [] procs []
-- Nothing
--
-- If a function is found with the given name, but no params match, then it does a fuzzy search
-- to all the overloaded functions' params using the form "param1, param2, param3, ..."
-- and shows the best match as hint.
--
-- >>> let procsDesc = [Function {pdParams = [RoutineParam {ppName="val"}, RoutineParam {ppName="param"}, RoutineParam {ppName="name"}]}, Function {pdParams = [RoutineParam {ppName="id"}, RoutineParam {ppName="attr"}]}]
--
-- >>> noRpcHint "api" "test" ["vall", "pqaram", "nam"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(name, param, val)"
--
-- >>> noRpcHint "api" "test" ["val", "param"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(name, param, val)"
--
-- >>> noRpcHint "api" "test" ["id", "attrs"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(attr, id)"
--
-- >>> noRpcHint "api" "test" ["id"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(attr, id)"
--
-- >>> noRpcHint "api" "test" ["noclosealternative"] procs procsDesc
-- Nothing
--
noRpcHint :: Text -> Text -> [Text] -> [QualifiedIdentifier] -> [Routine] -> Maybe Text
noRpcHint schema procName params allProcs overloadedProcs =
  fmap (("Perhaps you meant to call the function " <> schema <> ".") <>) possibleProcs
  where
    fuzzySetOfProcs  = Fuzzy.fromList [qiName k | k <- allProcs, qiSchema k == schema]
    fuzzySetOfParams = Fuzzy.fromList $ listToText <$> [[ppName prm | prm <- pdParams ov] | ov <- overloadedProcs]
    -- Cannot do a fuzzy search like: Fuzzy.getOne [[Text]] [Text], where [[Text]] is the list of params for each
    -- overloaded function and [Text] the given params. This converts those lists to text to make fuzzy search possible.
    -- E.g. ["val", "param", "name"] into "(name, param, val)"
    listToText       = ("(" <>) . (<> ")") . T.intercalate ", " . sort
    possibleProcs
      | null overloadedProcs = Fuzzy.getOne fuzzySetOfProcs procName
      | otherwise            = (procName <>) <$> Fuzzy.getOne fuzzySetOfParams (listToText params)

relHint :: [Relationship] -> Text
relHint rels = T.intercalate ", " (hintList <$> rels)
  where
    hintList Relationship{..} =
      let buildHint rel = "'" <> qiName relForeignTable <> "!" <> rel <> "'" in
      case relCardinality of
        M2M Junction{..} -> buildHint (qiName junTable)
        M2O cons _       -> buildHint cons
        O2O cons _ _     -> buildHint cons
        O2M cons _       -> buildHint cons
    -- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
    hintList ComputedRelationship{} = mempty

-- |
-- If no relationship is found then:
--
-- Looks for parent suggestions if parent not found
-- Looks for child suggestions if parent is found but child is not
-- Gives no suggestions if both are found (it means that there is a problem with the embed hint)
--
-- >>> :set -Wno-missing-fields
-- >>> let qi t = QualifiedIdentifier "api" t
-- >>> let rel ft = Relationship{relForeignTable = qi ft}
-- >>> let rels = HM.fromList [((qi "films", "api"), [rel "directors", rel "roles", rel "actors"])]
--
-- >>> noRelBetweenHint "film" "directors" "api" rels
-- Just "Perhaps you meant 'films' instead of 'film'."
--
-- >>> noRelBetweenHint "films" "role" "api" rels
-- Just "Perhaps you meant 'roles' instead of 'role'."
--
-- >>> noRelBetweenHint "films" "role" "api" rels
-- Just "Perhaps you meant 'roles' instead of 'role'."
--
-- >>> noRelBetweenHint "films" "actors" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "noclosealternative" "roles" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "films" "noclosealternative" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "films" "noclosealternative" "noclosealternative" rels
-- Nothing
--
noRelBetweenHint :: Text -> Text -> Schema -> RelationshipsMap -> Maybe Text
noRelBetweenHint parent child schema allRels = ("Perhaps you meant '" <>) <$>
  if isJust findParent
    then (<> "' instead of '" <> child <> "'.") <$> suggestChild
    else (<> "' instead of '" <> parent <> "'.") <$> suggestParent
  where
    findParent = HM.lookup (QualifiedIdentifier schema parent, schema) allRels
    fuzzySetOfParents  = Fuzzy.fromList [qiName (fst p) | p <- HM.keys allRels, snd p == schema]
    fuzzySetOfChildren = Fuzzy.fromList [qiName (relForeignTable c) | c <- fromMaybe [] findParent]
    suggestParent = Fuzzy.getOne fuzzySetOfParents parent
    -- Do not give suggestion if the child is found in the relations (weight = 1.0)
    suggestChild  = headMay [snd k | k <- Fuzzy.get fuzzySetOfChildren child, fst k < 1.0]

compressedRel :: Relationship -> JSON.Value
-- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
compressedRel ComputedRelationship{} = JSON.object mempty
compressedRel Relationship{..} =
  let
    fmtEls els = "(" <> T.intercalate ", " els <> ")"
  in
  JSON.object $
    ("embedding" JSON..= (qiName relTable <> " with " <> qiName relForeignTable :: Text))
    : case relCardinality of
        M2M Junction{..} -> [
            "cardinality" JSON..= ("many-to-many" :: Text)
          , "relationship" JSON..= (qiName junTable <> " using " <> junConstraint1 <> fmtEls (snd <$> junColsSource) <> " and " <> junConstraint2 <> fmtEls (snd <$> junColsTarget))
          ]
        M2O cons relColumns -> [
            "cardinality" JSON..= ("many-to-one" :: Text)
          , "relationship" JSON..= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
        O2O cons relColumns _ -> [
            "cardinality" JSON..= ("one-to-one" :: Text)
          , "relationship" JSON..= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
        O2M cons relColumns -> [
            "cardinality" JSON..= ("one-to-many" :: Text)
          , "relationship" JSON..= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
