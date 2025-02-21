{-|
Module      : PostgREST.Plan
Description : PostgREST Request Planner

This module is in charge of building an intermediate
representation between the HTTP request and the
final resulting SQL query.

A query tree is built in case of resource embedding. By inferring the
relationship between tables, join conditions are added for every embedded
resource.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgREST.Plan
  ( actionPlan
  , ActionPlan(..)
  , DbActionPlan(..)
  , InspectPlan(..)
  , InfoPlan(..)
  , CrudPlan(..)
  , CallReadPlan(..)
  ) where

import qualified Data.HashMap.Strict           as HM
import qualified Data.HashMap.Strict.InsOrd    as HMI
import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified PostgREST.SchemaCache.Routine as Routine

import Data.Either.Combinators (mapLeft, mapRight)
import Data.List               (delete, lookup)
import Data.Tree               (Tree (..))

import PostgREST.ApiRequest                  (Action (..),
                                              ApiRequest (..),
                                              DbAction (..),
                                              InvokeMethod (..),
                                              Mutation (..),
                                              Payload (..))
import PostgREST.Config                      (AppConfig (..))
import PostgREST.Error                       (ApiRequestError (..),
                                              Error (..))
import PostgREST.MediaType                   (MediaType (..))
import PostgREST.Query.SqlFragment           (sourceCTEName)
import PostgREST.RangeQuery                  (NonnegRange, allRange,
                                              convertToLimitZeroRange,
                                              restrictRange)
import PostgREST.SchemaCache                 (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers     (FieldName,
                                              QualifiedIdentifier (..),
                                              RelIdentifier (..),
                                              Schema)
import PostgREST.SchemaCache.Relationship    (Cardinality (..),
                                              Junction (..),
                                              Relationship (..),
                                              RelationshipsMap,
                                              relIsToOne)
import PostgREST.SchemaCache.Representations (DataRepresentation (..),
                                              RepresentationsMap)
import PostgREST.SchemaCache.Routine         (MediaHandler (..),
                                              MediaHandlerMap,
                                              ResolvedHandler,
                                              Routine (..),
                                              RoutineMap,
                                              RoutineParam (..),
                                              funcReturnsCompositeAlias,
                                              funcReturnsScalar,
                                              funcReturnsSetOfScalar)
import PostgREST.SchemaCache.Table           (Column (..), Table (..),
                                              TablesMap,
                                              tableColumnsList,
                                              tablePKCols)

import PostgREST.ApiRequest.Preferences
import PostgREST.ApiRequest.Types
import PostgREST.Plan.CallPlan
import PostgREST.Plan.MutatePlan
import PostgREST.Plan.ReadPlan          as ReadPlan
import PostgREST.Plan.Types

import qualified Hasql.Transaction.Sessions       as SQL
import qualified PostgREST.ApiRequest.QueryParams as QueryParams
import qualified PostgREST.MediaType              as MediaType

import Protolude hiding (from)

-- $setup
-- Setup for doctests
-- >>> import Data.Ranged.Ranges (fullRange)

data CrudPlan
  = WrappedReadPlan
  { wrReadPlan :: ReadPlanTree
  , pTxMode    :: SQL.Mode
  , wrHandler  :: MediaHandler
  , wrMedia    :: MediaType
  , wrHdrsOnly :: Bool
  , crudQi     :: QualifiedIdentifier
  }
  | MutateReadPlan {
    mrReadPlan   :: ReadPlanTree
  , mrMutatePlan :: MutatePlan
  , pTxMode      :: SQL.Mode
  , mrHandler    :: MediaHandler
  , mrMedia      :: MediaType
  , mrMutation   :: Mutation
  , crudQi       :: QualifiedIdentifier
  }

data CallReadPlan = CallReadPlan {
    crReadPlan :: ReadPlanTree
  , crCallPlan :: CallPlan
  , crTxMode   :: SQL.Mode
  , crProc     :: Routine
  , crHandler  :: MediaHandler
  , crMedia    :: MediaType
  , crInvMthd  :: InvokeMethod
  , crQi       :: QualifiedIdentifier
  }

data InspectPlan = InspectPlan {
    ipMedia    :: MediaType
  , ipTxmode   :: SQL.Mode
  , ipHdrsOnly :: Bool
  , ipSchema   :: Schema
  }

data DbActionPlan = DbCrud CrudPlan | DbCall CallReadPlan | MaybeDb InspectPlan
data InfoPlan     = RelInfoPlan QualifiedIdentifier | RoutineInfoPlan CallReadPlan | SchemaInfoPlan
data ActionPlan   = Db DbActionPlan | NoDb InfoPlan

actionPlan :: Action -> AppConfig -> ApiRequest -> SchemaCache -> Either Error ActionPlan
actionPlan act conf apiReq sCache = case act of
    ActDb dbAct              -> Db <$> dbActionPlan dbAct conf apiReq sCache
    ActRelationInfo ident    -> pure . NoDb $ RelInfoPlan ident
    ActRoutineInfo ident inv -> NoDb . RoutineInfoPlan <$> callReadPlan ident conf sCache apiReq inv
    ActSchemaInfo            -> pure $ NoDb SchemaInfoPlan

dbActionPlan :: DbAction -> AppConfig -> ApiRequest -> SchemaCache -> Either Error DbActionPlan
dbActionPlan dbAct conf apiReq sCache = case dbAct of
  ActRelationRead identifier headersOnly ->
    DbCrud <$> wrappedReadPlan identifier conf sCache apiReq headersOnly
  ActRelationMut identifier mut ->
    DbCrud <$> mutateReadPlan mut apiReq identifier conf sCache
  ActRoutine identifier invMethod ->
    DbCall <$> callReadPlan identifier conf sCache apiReq invMethod
  ActSchemaRead tSchema headersOnly ->
    MaybeDb <$> inspectPlan apiReq headersOnly tSchema

wrappedReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Bool -> Either Error CrudPlan
wrappedReadPlan  identifier conf sCache apiRequest@ApiRequest{iPreferences=Preferences{..},..} headersOnly = do
  qi <- mapLeft ApiRequestError $ findTable identifier (dbTables sCache)
  rPlan <- readPlan qi conf sCache apiRequest
  (handler, mediaType)  <- mapLeft ApiRequestError $ negotiateContent conf apiRequest qi iAcceptMediaType (dbMediaHandlers sCache) (hasDefaultSelect rPlan)
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestError $ InvalidPreferences invalidPrefs else Right ()
  return $ WrappedReadPlan rPlan SQL.Read handler mediaType headersOnly qi

mutateReadPlan :: Mutation -> ApiRequest -> QualifiedIdentifier -> AppConfig -> SchemaCache -> Either Error CrudPlan
mutateReadPlan  mutation apiRequest@ApiRequest{iPreferences=Preferences{..},..} identifier conf sCache = do
  qi <- mapLeft ApiRequestError $ findTable identifier (dbTables sCache)
  rPlan <- readPlan qi conf sCache apiRequest
  mPlan <- mutatePlan mutation qi apiRequest sCache rPlan
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestError $ InvalidPreferences invalidPrefs else Right ()
  (handler, mediaType)  <- mapLeft ApiRequestError $ negotiateContent conf apiRequest qi iAcceptMediaType (dbMediaHandlers sCache) (hasDefaultSelect rPlan)
  return $ MutateReadPlan rPlan mPlan SQL.Write handler mediaType mutation qi

callReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> InvokeMethod -> Either Error CallReadPlan
callReadPlan identifier conf sCache apiRequest@ApiRequest{iPreferences=Preferences{preferHandling, invalidPrefs},..} invMethod = do
  let paramKeys = case invMethod of
        InvRead _ -> S.fromList $ fst <$> qsParams'
        Inv       -> iColumns
  proc@Function{..} <- mapLeft ApiRequestError $
    findProc identifier paramKeys (dbRoutines sCache) iContentMediaType (invMethod == Inv)
  let relIdentifier = QualifiedIdentifier pdSchema (fromMaybe pdName $ Routine.funcTableName proc) -- done so a set returning function can embed other relations
  rPlan <- readPlan relIdentifier conf sCache apiRequest
  let args = case (invMethod, iContentMediaType) of
        (InvRead _, _)      -> DirectArgs $ toRpcParams proc qsParams'
        (Inv, MTUrlEncoded) -> DirectArgs $ maybe mempty (toRpcParams proc . payArray) iPayload
        (Inv, _)            -> JsonArgs $ payRaw <$> iPayload
      txMode = case (invMethod, pdVolatility) of
          (InvRead _,  _)          -> SQL.Read
          (Inv, Routine.Stable)    -> SQL.Read
          (Inv, Routine.Immutable) -> SQL.Read
          (Inv, Routine.Volatile)  -> SQL.Write
      cPlan = callPlan proc apiRequest paramKeys args rPlan
  (handler, mediaType)  <- mapLeft ApiRequestError $ negotiateContent conf apiRequest relIdentifier iAcceptMediaType (dbMediaHandlers sCache) (hasDefaultSelect rPlan)
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestError $ InvalidPreferences invalidPrefs else Right ()
  return $ CallReadPlan rPlan cPlan txMode proc handler mediaType invMethod identifier
  where
    qsParams' = QueryParams.qsParams iQueryParams

hasDefaultSelect :: ReadPlanTree -> Bool
hasDefaultSelect (Node ReadPlan{select=[CoercibleSelectField{csField=CoercibleField{cfName}}]} []) = cfName == "*"
hasDefaultSelect _ = False

inspectPlan :: ApiRequest -> Bool -> Schema -> Either Error InspectPlan
inspectPlan apiRequest headersOnly schema = do
  let producedMTs = [MTOpenAPI, MTApplicationJSON, MTAny]
      accepts     = iAcceptMediaType apiRequest
  mediaType <- if not . null $ L.intersect accepts producedMTs
    then Right MTOpenAPI
    else Left . ApiRequestError . MediaTypeError $ MediaType.toMime <$> accepts
  return $ InspectPlan mediaType SQL.Read headersOnly schema

{-|
  Search a pg proc by matching name and arguments keys to parameters. Since a function can be overloaded,
  the name is not enough to find it. An overloaded function can have a different volatility or even a different return type.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> RoutineMap -> MediaType -> Bool -> Either ApiRequestError Routine
findProc qi argumentsKeys allProcs contentMediaType isInvPost =
  case matchProc of
    ([], [])     -> Left $ NoRpc (qiSchema qi) (qiName qi) (S.toList argumentsKeys) contentMediaType isInvPost (HM.keys allProcs) lookupProcName
    -- If there are no functions with named arguments, fallback to the single unnamed argument function
    ([], [proc]) -> Right proc
    ([], procs)  -> Left $ AmbiguousRpc (toList procs)
    -- Matches the functions with named arguments
    ([proc], _)  -> Right proc
    (procs, _)   -> Left $ AmbiguousRpc (toList procs)
  where
    matchProc = overloadedProcPartition lookupProcName
    -- First find the proc by name
    lookupProcName = HM.lookupDefault mempty qi allProcs
    -- The partition obtained has the form (overloadedProcs,fallbackProcs)
    -- where fallbackProcs are functions with a single unnamed parameter
    overloadedProcPartition = foldr select ([],[])
    select proc ~(ts,fs)
      | matchesParams proc         = (proc:ts,fs)
      | hasSingleUnnamedParam proc = (ts,proc:fs)
      | otherwise                  = (ts,fs)
    -- If the function is called with post and has a single unnamed parameter
    -- it can be called depending on content type and the parameter type
    hasSingleUnnamedParam Function{pdParams=[RoutineParam{ppType}]} = isInvPost && case (contentMediaType, ppType) of
      (MTApplicationJSON, "json")  -> True
      (MTApplicationJSON, "jsonb") -> True
      (MTTextPlain, "text")        -> True
      (MTTextXML, "xml")           -> True
      (MTOctetStream, "bytea")     -> True
      _                            -> False
    hasSingleUnnamedParam _ = False
    matchesParams proc =
      let
        params = pdParams proc
      in
      -- If the function has no parameters, the arguments keys must be empty as well
      if null params
        then null argumentsKeys && not (isInvPost && contentMediaType `elem` [MTOctetStream, MTTextPlain, MTTextXML])
      -- A function has optional and required parameters. Optional parameters have a default value and
      -- don't require arguments for the function to be executed, required parameters must have an argument present.
      else case L.partition ppReq params of
      -- If the function only has required parameters, the arguments keys must match those parameters
        (reqParams, [])        -> argumentsKeys == S.fromList (ppName <$> reqParams)
      -- If the function only has optional parameters, the arguments keys can match none or any of them(a subset)
        ([], optParams)        -> argumentsKeys `S.isSubsetOf` S.fromList (ppName <$> optParams)
      -- If the function has required and optional parameters, the arguments keys have to match the required parameters
      -- and can match any or none of the default parameters.
        (reqParams, optParams) -> argumentsKeys `S.difference` S.fromList (ppName <$> optParams) == S.fromList (ppName <$> reqParams)

-- | During planning we need to resolve Field -> CoercibleField (finding the context specific target type and map function).
-- | ResolverContext facilitates this without the need to pass around a laundry list of parameters.
data ResolverContext = ResolverContext
  { tables          :: TablesMap
  , representations :: RepresentationsMap
  , qi              :: QualifiedIdentifier  -- ^ The table we're currently attending; changes as we recurse into joins etc.
  , outputType      :: Text                 -- ^ The output type for the response payload; e.g. "csv", "json", "binary".
  }

resolveColumnField :: Column -> Maybe ToTsVector -> CoercibleField
resolveColumnField col toTsV = CoercibleField (colName col) mempty False toTsV (colNominalType col) Nothing (colDefault col) False

resolveTableFieldName :: Table -> FieldName -> Maybe ToTsVector -> CoercibleField
resolveTableFieldName table fieldName toTsV=
  fromMaybe (unknownField fieldName []) $ HMI.lookup fieldName (tableColumns table) >>=
    Just . flip resolveColumnField toTsV

-- | Resolve a type within the context based on the given field name and JSON path. Although there are situations where failure to resolve a field is considered an error (see `resolveOrError`), there are also situations where we allow it (RPC calls). If it should be an error and `resolveOrError` doesn't fit, ensure to check the `cfIRType` isn't empty.
resolveTypeOrUnknown :: ResolverContext -> Field -> Maybe ToTsVector -> CoercibleField
resolveTypeOrUnknown ResolverContext{..} (fn, jp) toTsV =
  case res of
    -- types that are already json/jsonb don't need to be converted with `to_jsonb` for using arrow operators `data->attr`
    -- this prevents indexes not applying https://github.com/PostgREST/postgrest/issues/2594
    cf@CoercibleField{cfIRType="json"}     -> cf{cfJsonPath=jp, cfToJson=False}
    cf@CoercibleField{cfIRType="jsonb"}    -> cf{cfJsonPath=jp, cfToJson=False}
    -- Do not apply to_tsvector to tsvector types
    cf@CoercibleField{cfIRType="tsvector"} -> cf{cfJsonPath=jp, cfToJson=True, cfToTsVector=Nothing}
    -- other types will get converted `to_jsonb(col)->attr`, even unknown types
    cf                                     -> cf{cfJsonPath=jp, cfToJson=True}
  where
    res = fromMaybe (unknownField fn jp) $ HM.lookup qi tables >>=
          Just . (\t -> resolveTableFieldName t fn toTsV)

-- | Install any pre-defined data representation from source to target to coerce this reference.
--
-- Note that we change the IR type here. This might seem unintuitive. The short of it is that for a CoercibleField without a transformer, input type == output type. A transformer maps from a -> b, so by definition the input type will be a and the output type b after. And cfIRType is the *input* type.
--
-- It might feel odd that once a transformer is added we 'forget' the target type (because now a /= b). You might also note there's no obvious way to stack transforms (even if there was a stack, you erased what type you're working with so it's awkward). Alas as satisfying as it would be to engineer a layered mapping system with full type information, we just don't need it.
withTransformer :: ResolverContext -> Text -> Text -> CoercibleField -> CoercibleField
withTransformer ResolverContext{representations} sourceType targetType field =
  fromMaybe field $ HM.lookup (sourceType, targetType) representations >>=
    (\fieldRepresentation -> Just field{cfIRType=sourceType, cfTransform=Just (drFunction fieldRepresentation)})

-- | Map the intermediate representation type to the output type, if available.
withOutputFormat :: ResolverContext -> CoercibleField -> CoercibleField
withOutputFormat ctx@ResolverContext{outputType} field@CoercibleField{cfIRType} = withTransformer ctx cfIRType outputType field

-- | Map text into the intermediate representation type, if available.
withTextParse :: ResolverContext -> CoercibleField -> CoercibleField
withTextParse ctx field@CoercibleField{cfIRType} = withTransformer ctx "text" cfIRType field

-- | Map json into the intermediate representation type, if available.
withJsonParse :: ResolverContext -> CoercibleField -> CoercibleField
withJsonParse ctx field@CoercibleField{cfIRType} = withTransformer ctx "json" cfIRType field

-- | Map the intermediate representation type to the output type defined by the resolver context (normally json), if available.
resolveOutputField :: ResolverContext -> Field -> CoercibleField
resolveOutputField ctx field = withOutputFormat ctx $ resolveTypeOrUnknown ctx field Nothing

-- | Map the query string format of a value (text) into the intermediate representation type, if available.
resolveQueryInputField :: ResolverContext -> Field -> OpExpr -> CoercibleField
resolveQueryInputField ctx field opExpr = withTextParse ctx $ resolveTypeOrUnknown ctx field toTsVector
  where
    toTsVector = case opExpr of
      OpExpr _ (Fts _ lang _) -> Just $ ToTsVector lang
      _                       -> Nothing

-- | Builds the ReadPlan tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Either Error ReadPlanTree
readPlan qi@QualifiedIdentifier{..} AppConfig{configDbMaxRows, configDbAggregates} SchemaCache{dbTables, dbRelationships, dbRepresentations} apiRequest  =
  let
    -- JSON output format hardcoded for now. In the future we might want to support other output mappings such as CSV.
    ctx = ResolverContext dbTables dbRepresentations qi "json"
  in
    mapLeft ApiRequestError $
    treeRestrictRange configDbMaxRows (iAction apiRequest) =<<
    hoistSpreadAggFunctions =<<
    validateAggFunctions configDbAggregates =<<
    addRelSelects =<<
    addNullEmbedFilters =<<
    validateSpreadEmbeds =<<
    addRelatedOrders =<<
    addAliases =<<
    expandStars ctx =<<
    addRels qiSchema (iAction apiRequest) dbRelationships Nothing =<<
    addLogicTrees ctx apiRequest =<<
    addRanges apiRequest =<<
    addOrders ctx apiRequest =<<
    addFilters ctx apiRequest (initReadRequest ctx $ QueryParams.qsSelect $ iQueryParams apiRequest)

-- Build the initial read plan tree
initReadRequest :: ResolverContext -> [Tree SelectItem] -> ReadPlanTree
initReadRequest ctx@ResolverContext{qi=QualifiedIdentifier{..}} =
  foldr (treeEntry rootDepth) $ Node defReadPlan{from=qi ctx, relName=qiName, depth=rootDepth} []
  where
    rootDepth = 0
    defReadPlan = ReadPlan [] (QualifiedIdentifier mempty mempty) Nothing [] [] allRange mempty Nothing [] Nothing mempty Nothing Nothing False [] rootDepth
    treeEntry :: Depth -> Tree SelectItem -> ReadPlanTree -> ReadPlanTree
    treeEntry depth (Node si fldForest) (Node q rForest) =
      let nxtDepth = succ depth in
      case si of
        SelectRelation{..} ->
          Node q $
            foldr (treeEntry nxtDepth)
            (Node defReadPlan{from=QualifiedIdentifier qiSchema selRelation, relName=selRelation, relAlias=selAlias, relHint=selHint, relJoinType=selJoinType, depth=nxtDepth} [])
            fldForest:rForest
        SpreadRelation{..} ->
          Node q $
            foldr (treeEntry nxtDepth)
            (Node defReadPlan{from=QualifiedIdentifier qiSchema selRelation, relName=selRelation, relHint=selHint, relJoinType=selJoinType, depth=nxtDepth, relIsSpread=True} [])
            fldForest:rForest
        SelectField{..} ->
          Node q{select=CoercibleSelectField (resolveOutputField ctx{qi=from q} selField) selAggregateFunction selAggregateCast selCast selAlias:select q} rForest

-- If an alias is explicitly specified, it is always respected. However, an alias may be
-- determined automatically in the case of a select term with a JSON path, or in the case
-- of domain representations.
addAliases :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addAliases = Right . fmap addAliasToPlan
  where
    addAliasToPlan rp@ReadPlan{select=sel} = rp{select=map aliasSelectField sel}

    aliasSelectField :: CoercibleSelectField -> CoercibleSelectField
    aliasSelectField field@CoercibleSelectField{csField=fieldDetails, csAggFunction=aggFun, csAlias=alias}
      | isJust alias = field
      | isJust aggFun = fieldAliasForSpreadAgg field
      | isJsonKeyPath fieldDetails, Just key <- lastJsonKey fieldDetails = field { csAlias = Just key }
      | isTransformPath fieldDetails = field { csAlias = Just (cfName fieldDetails) }
      | otherwise = field

    -- A request like: `/top_table?select=...middle_table(...nested_table(count()))` will `SELECT` the full row instead of `*`,
    -- because doing a `COUNT(*)` in `top_table` would not return the desired results.
    -- So we use the "count" alias if none is present since the field name won't be selected.
    fieldAliasForSpreadAgg field
      | cfFullRow (csField field) = field { csAlias = Just "count" }
      | otherwise                 = field

    isJsonKeyPath CoercibleField{cfJsonPath=(_: _)} = True
    isJsonKeyPath _                                 = False

    isTransformPath CoercibleField{cfTransform=(Just _), cfName=_} = True
    isTransformPath _ = False

    lastJsonKey CoercibleField{cfName=fieldName, cfJsonPath=jsonPath} =
      case jOp <$> lastMay jsonPath of
            Just (JKey key) -> Just key
            Just (JIdx _)   -> Just $ fromMaybe fieldName lastKey
              -- We get the lastKey because on:
              -- `select=data->1->mycol->>2`, we need to show the result as [ {"mycol": ..}, {"mycol": ..} ]
              -- `select=data->3`, we need to show the result as [ {"data": ..}, {"data": ..} ]
              where lastKey = jVal <$> find (\case JKey{} -> True; _ -> False) (jOp <$> reverse jsonPath)
            Nothing -> Nothing

knownColumnsInContext :: ResolverContext -> [Column]
knownColumnsInContext ResolverContext{..} =
  fromMaybe [] $ HM.lookup qi tables >>=
  Just . tableColumnsList

-- | Expand "select *" into explicit field names of the table in the following situations:
-- * When there are data representations present.
-- * When there is an aggregate function in a given ReadPlan or its parent.
expandStars :: ResolverContext -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
expandStars ctx rPlanTree = Right $ expandStarsForReadPlan False rPlanTree
  where
    expandStarsForReadPlan :: Bool -> ReadPlanTree -> ReadPlanTree
    expandStarsForReadPlan hasAgg (Node rp@ReadPlan{select, from=fromQI, fromAlias=alias} children) =
      let
        newHasAgg = hasAgg || any (isJust . csAggFunction) select
        newCtx = adjustContext ctx fromQI alias
        newRPlan = expandStarsForTable newCtx newHasAgg rp
      in Node newRPlan (map (expandStarsForReadPlan newHasAgg) children)

    -- Choose the appropriate context based on whether we're dealing with "pgrst_source"
    adjustContext :: ResolverContext -> QualifiedIdentifier -> Maybe Text -> ResolverContext
    -- When the schema is "" and the table is the source CTE, we assume the true source table is given in the from
    -- alias and belongs to the request schema. See the bit in `addRels` with `newFrom = ...`.
    adjustContext context@ResolverContext{qi=ctxQI} (QualifiedIdentifier "" "pgrst_source") (Just a) = context{qi=ctxQI{qiName=a}}
    adjustContext context fromQI _ = context{qi=fromQI}

expandStarsForTable :: ResolverContext -> Bool -> ReadPlan -> ReadPlan
expandStarsForTable ctx@ResolverContext{representations, outputType} hasAgg rp@ReadPlan{select=selectFields, relIsSpread=isSpread}
  -- We expand if either of the below are true:
  -- * We have a '*' select AND there is an aggregate function in this ReadPlan's sub-tree.
  -- * We have a '*' select AND the target table has at least one data representation.
  -- We ignore '*' selects that have an aggregate function attached, unless it's a `COUNT(*)` for a Spread Embed,
  -- we tag it as "full row" in that case.
  | hasStarSelect && (hasAgg || hasDataRepresentation) = rp{select = concatMap (expandStarSelectField isSpread knownColumns) selectFields}
  | otherwise = rp
  where
    hasStarSelect = "*" `elem` map (cfName . csField) filteredSelectFields
    filteredSelectFields = filter (shouldExpandOrTag . csAggFunction) selectFields
    shouldExpandOrTag aggFunc = isNothing aggFunc || (isSpread && aggFunc == Just Count)
    hasDataRepresentation = any hasOutputRep knownColumns
    knownColumns = knownColumnsInContext ctx

    hasOutputRep :: Column -> Bool
    hasOutputRep col = HM.member (colNominalType col, outputType) representations

    expandStarSelectField :: Bool -> [Column] -> CoercibleSelectField -> [CoercibleSelectField]
    expandStarSelectField _ columns sel@CoercibleSelectField{csField=CoercibleField{cfName="*", cfJsonPath=[]}, csAggFunction=Nothing} =
      map (\col -> sel { csField = withOutputFormat ctx $ resolveColumnField col Nothing }) columns
    expandStarSelectField True _ sel@CoercibleSelectField{csField=fld@CoercibleField{cfName="*", cfJsonPath=[]}, csAggFunction=Just Count} =
      [sel { csField = fld { cfFullRow = True } }]
    expandStarSelectField _ _ selectField = [selectField]

-- | Enforces the `max-rows` config on the result
treeRestrictRange :: Maybe Integer -> Action -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
treeRestrictRange _ (ActDb (ActRelationMut _ _)) request = Right request
treeRestrictRange maxRows _ request = pure $ nodeRestrictRange maxRows <$> request
  where
    nodeRestrictRange :: Maybe Integer -> ReadPlan -> ReadPlan
    nodeRestrictRange m q@ReadPlan{range_=r} = q{range_= convertToLimitZeroRange r (restrictRange m r) }

-- add relationships to the nodes of the tree by traversing the forest while keeping track of the parentNode(https://stackoverflow.com/questions/22721064/get-the-parent-of-a-node-in-data-tree-haskell#comment34627048_22721064)
-- also adds aliasing
addRels :: Schema -> Action -> RelationshipsMap -> Maybe ReadPlanTree -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRels schema action allRels parentNode (Node rPlan@ReadPlan{relName,relHint,relAlias,depth} forest) =
  case parentNode of
    Just (Node ReadPlan{from=parentNodeQi, fromAlias=parentAlias} _) ->
      let
        newReadPlan = (\r ->
          let newAlias = Just (qiName (relForeignTable r) <> "_" <> show depth)
              aggAlias = qiName (relTable r) <> "_" <> fromMaybe relName relAlias <> "_" <> show depth in
          case r of
            Relationship{relCardinality=M2M _} -> -- m2m does internal implicit joins that don't need aliasing
              rPlan{from=relForeignTable r, relToParent=Just r, relAggAlias=aggAlias, relJoinConds=getJoinConditions Nothing parentAlias r}
            ComputedRelationship{} ->
              rPlan{from=relForeignTable r, relToParent=Just r{relTableAlias=maybe (relTable r) (QualifiedIdentifier mempty) parentAlias}, relAggAlias=aggAlias, fromAlias=newAlias}
            _ ->
              rPlan{from=relForeignTable r, relToParent=Just r, relAggAlias=aggAlias, fromAlias=newAlias, relJoinConds=getJoinConditions newAlias parentAlias r}
          ) <$> rel
        origin = if depth == 1 -- Only on depth 1 we check if the root(depth 0) has an alias so the sourceCTEName alias can be found as a relationship
          then fromMaybe (qiName parentNodeQi) parentAlias
          else qiName parentNodeQi
        rel = findRel schema allRels origin relName relHint
      in
      Node <$> newReadPlan <*> (updateForest . hush $ Node <$> newReadPlan <*> pure forest)
    Nothing -> -- root case
      let
        newFrom  = QualifiedIdentifier mempty sourceCTEName
        newAlias = Just (qiName $ from rPlan)
        newReadPlan = case action of
          -- the CTE for mutations/rpc is used as WITH sourceCTEName .. SELECT .. FROM sourceCTEName as alias,
          -- we use the table name as an alias so findRel can find the right relationship.
          ActDb (ActRelationMut _ _) -> rPlan{from=newFrom, fromAlias=newAlias}
          ActDb (ActRoutine _ _)     -> rPlan{from=newFrom, fromAlias=newAlias}
          _                  -> rPlan
      in
      Node newReadPlan <$> updateForest (Just $ Node newReadPlan forest)
  where
    updateForest :: Maybe ReadPlanTree -> Either ApiRequestError [ReadPlanTree]
    updateForest rq = addRels schema action allRels rq `traverse` forest

getJoinConditions :: Maybe Alias -> Maybe Alias -> Relationship -> [JoinCondition]
getJoinConditions _ _ ComputedRelationship{} = []
getJoinConditions tblAlias parentAlias Relationship{relTable=qi,relForeignTable=fQi,relCardinality=card} =
  case card of
    M2M (Junction QualifiedIdentifier{qiName=jtn} _ _ jcols1 jcols2) ->
      (toJoinCondition Nothing Nothing ftN jtn <$> jcols2) ++ (toJoinCondition parentAlias tblAlias tN jtn <$> jcols1)
    O2M _ cols ->
      toJoinCondition parentAlias tblAlias tN ftN <$> cols
    M2O _ cols ->
      toJoinCondition parentAlias tblAlias tN ftN <$> cols
    O2O _ cols _ ->
      toJoinCondition parentAlias tblAlias tN ftN <$> cols
  where
    QualifiedIdentifier{qiSchema=tSchema, qiName=tN} = qi
    QualifiedIdentifier{qiName=ftN} = fQi
    toJoinCondition :: Maybe Alias -> Maybe Alias -> Text -> Text -> (FieldName, FieldName) -> JoinCondition
    toJoinCondition prAl newAl tb ftb (c, fc) =
      let qi1 = QualifiedIdentifier tSchema ftb
          qi2 = QualifiedIdentifier tSchema tb in
        JoinCondition (maybe qi1 (QualifiedIdentifier mempty) newAl, fc)
                      (maybe qi2 (QualifiedIdentifier mempty) prAl, c)

-- Finds a relationship between an origin and a target in the request:
-- /origin?select=target(*) If more than one relationship is found then the
-- request is ambiguous and we return an error.  In that case the request can
-- be disambiguated by adding precision to the target or by using a hint:
-- /origin?select=target!hint(*). The origin can be a table or view.
findRel :: Schema -> RelationshipsMap -> NodeName -> NodeName -> Maybe Hint -> Either ApiRequestError Relationship
findRel schema allRels origin target hint =
  case rels of
    []  -> Left $ NoRelBetween origin target hint schema allRels
    [r] -> Right r
    rs  -> Left $ AmbiguousRelBetween origin target rs
  where
    matchFKSingleCol hint_ card = case card of
      O2M{relColumns=[(col, _)]} -> hint_ == col
      M2O{relColumns=[(col, _)]} -> hint_ == col
      O2O{relColumns=[(col, _)]} -> hint_ == col
      _                          -> False
    matchFKRefSingleCol hint_ card  = case card of
      O2M{relColumns=[(_, fCol)]} -> hint_ == fCol
      M2O{relColumns=[(_, fCol)]} -> hint_ == fCol
      O2O{relColumns=[(_, fCol)]} -> hint_ == fCol
      _                           -> False
    matchConstraint tar card = case card of
      O2M{relCons} -> tar == relCons
      M2O{relCons} -> tar == relCons
      O2O{relCons} -> tar == relCons
      _            -> False
    matchJunction hint_ card = case card of
      M2M Junction{junTable} -> hint_ == qiName junTable
      _                      -> False
    isM2O card = case card of
      M2O _ _ -> True
      _       -> False
    isO2M card = case card of
      O2M _ _ -> True
      _       -> False
    rels = filter (\case
      ComputedRelationship{relFunction} -> target == qiName relFunction
      Relationship{..} ->
        -- In a self-relationship we have a single foreign key but two relationships with different cardinalities: M2O/O2M. For disambiguation, we use the convention of getting:
        -- TODO: handle one-to-one and many-to-many self-relationships
        if relIsSelf
        then case hint of
          Nothing ->
            -- The O2M by using the table name in the target
            target == qiName relForeignTable && isO2M relCardinality -- /family_tree?select=children:family_tree(*)
            ||
            -- The M2O by using the column name in the target
            matchFKSingleCol target relCardinality && isM2O relCardinality -- /family_tree?select=parent(*)
          Just hnt ->
            -- /organizations?select=auditees:organizations!auditor(*)
            target == qiName relForeignTable && isO2M relCardinality
            && matchFKRefSingleCol hnt relCardinality -- auditor
        else case hint of
          -- DEPRECATED(remove after 2 major releases since v11.1.0): remove target
          -- target = table / view / constraint / column-from-origin (constraint/column-from-origin can only come from tables https://github.com/PostgREST/postgrest/issues/2277)
          -- DEPRECATED(remove after 2 major releases since v11.1.0): remove hint as table/view/columns and only leave it as constraint
          -- hint   = table / view / constraint / column-from-origin / column-from-target (hint can take table / view values to aid in finding the junction in an m2m relationship)
          Nothing ->
              -- /projects?select=clients(*)
              target == qiName relForeignTable -- clients
              ||
              -- /projects?select=projects_client_id_fkey(*)
              matchConstraint target relCardinality -- projects_client_id_fkey
              && not relFTableIsView
              ||
              -- /projects?select=client_id(*)
              matchFKSingleCol target relCardinality -- client_id
              && not relFTableIsView
          Just hnt ->
            -- /projects?select=clients(*)
            target == qiName relForeignTable -- clients
            && (
              -- /projects?select=clients!projects_client_id_fkey(*)
              matchConstraint hnt relCardinality || -- projects_client_id_fkey

              -- /projects?select=clients!client_id(*) or /projects?select=clients!id(*)
              matchFKSingleCol hnt relCardinality      || -- client_id
              matchFKRefSingleCol hnt relCardinality   || -- id

              -- /users?select=tasks!users_tasks(*) many-to-many between users and tasks
              matchJunction hnt relCardinality -- users_tasks
            )
      ) $ fromMaybe mempty $ HM.lookup (QualifiedIdentifier schema origin, schema) allRels


addRelSelects :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRelSelects node@(Node rp forest)
  | null forest = Right node
  | otherwise   =
    let newForest     = rights $ addRelSelects <$> forest
        newRelSelects = mapMaybe generateRelSelectField newForest
    in Right $ Node rp { relSelect = newRelSelects } newForest

generateRelSelectField :: ReadPlanTree -> Maybe RelSelectField
generateRelSelectField (Node rp@ReadPlan{relToParent=Just _, relAggAlias, relIsSpread = True} _) =
  Just $ Spread { rsSpreadSel = generateSpreadSelectFields rp, rsAggAlias = relAggAlias }
generateRelSelectField (Node ReadPlan{relToParent=Just rel, select, relName, relAlias, relAggAlias, relIsSpread = False} forest) =
  Just $ JsonEmbed { rsEmbedMode, rsSelName, rsAggAlias = relAggAlias, rsEmptyEmbed }
  where
    rsSelName = fromMaybe relName relAlias
    rsEmbedMode = if relIsToOne rel then JsonObject else JsonArray
    rsEmptyEmbed = hasOnlyNullEmbed (null select) forest
    hasOnlyNullEmbed = foldr checkIfNullEmbed
    checkIfNullEmbed :: ReadPlanTree -> Bool -> Bool
    checkIfNullEmbed (Node ReadPlan{select=s} f) isNullEmbed =
      isNullEmbed && hasOnlyNullEmbed (null s) f
generateRelSelectField _ = Nothing

generateSpreadSelectFields :: ReadPlan -> [SpreadSelectField]
generateSpreadSelectFields ReadPlan{select, relSelect} =
  -- We combine the select and relSelect fields into a single list of SpreadSelectField.
  selectSpread ++ relSelectSpread
  where
    selectSpread = map selectToSpread select
    selectToSpread :: CoercibleSelectField -> SpreadSelectField
    selectToSpread CoercibleSelectField{csField = CoercibleField{cfName}, csAlias} =
      SpreadSelectField { ssSelName = fromMaybe cfName csAlias, ssSelAggFunction = Nothing, ssSelAggCast = Nothing, ssSelAlias = Nothing }

    relSelectSpread = concatMap relSelectToSpread relSelect
    relSelectToSpread :: RelSelectField -> [SpreadSelectField]
    relSelectToSpread (JsonEmbed{rsSelName}) =
      [SpreadSelectField { ssSelName = rsSelName, ssSelAggFunction = Nothing, ssSelAggCast = Nothing, ssSelAlias = Nothing }]
    relSelectToSpread (Spread{rsSpreadSel}) =
      rsSpreadSel

-- When aggregates are present in a ReadPlan that will be spread, we "hoist"
-- to the highest level possible so that their semantics make sense. For instance,
-- imagine the user performs the following request:
-- `GET /projects?select=client_id,...project_invoices(invoice_total.sum())`
--
-- In this case, it is sensible that we would expect to receive the sum of the
-- `invoice_total`, grouped by the `client_id`. Without hoisting, the sum would
-- be performed in the sub-query for the joined table `project_invoices`, thus
-- making it essentially a no-op. With hoisting, we hoist the aggregate function
-- so that the aggregate function is performed in a more sensible context.
--
-- We will try to hoist the aggregate function to the highest possible level,
-- which means that we hoist until we reach the root node, or until we reach a
-- ReadPlan that will be embedded a JSON object or JSON array.

-- This type alias represents an aggregate that is to be hoisted to the next
-- level up. The first tuple of `Alias` and `FieldName` contain the alias for
-- the joined table and the original field name for the hoisted field.
--
-- The second tuple contains the aggregate function to be applied, the cast, and
-- the alias, if it was supplied by the user or otherwise determined.
type HoistedAgg = ((Alias, FieldName), (AggregateFunction, Maybe Cast, Maybe Alias))

hoistSpreadAggFunctions :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
hoistSpreadAggFunctions tree = Right $ fst $ applySpreadAggHoistingToNode tree

applySpreadAggHoistingToNode :: ReadPlanTree -> (ReadPlanTree, [HoistedAgg])
applySpreadAggHoistingToNode (Node rp@ReadPlan{relAggAlias, relToParent, relIsSpread} children) =
  let (newChildren, childAggLists) = unzip $ map applySpreadAggHoistingToNode children
      allChildAggLists = concat childAggLists
      (newSelects, aggList) = if depth rp == 0 || (isJust relToParent && not relIsSpread)
                                then (select rp, [])
                                else hoistFromSelectFields relAggAlias (select rp)

      -- If the current `ReadPlan` is a spread rel and it has aggregates hoisted from
      -- child relationships, then it must hoist those aggregates to its parent rel.
      -- So we update them with the current `relAggAlias`.
      hoistAgg ((_, fieldName), hoistFunc) = ((relAggAlias, fieldName), hoistFunc)
      hoistedAggList = if relIsSpread
                       then aggList ++ map hoistAgg allChildAggLists
                       else aggList

      newRelSelects = if null children || relIsSpread
                      then relSelect rp
                      else map (hoistIntoRelSelectFields allChildAggLists) $ relSelect rp
  in  (Node rp { select = newSelects, relSelect = newRelSelects } newChildren, hoistedAggList)

-- Hoist aggregate functions from the select list of a ReadPlan, and return the
-- updated select list and the list of hoisted aggregates.
hoistFromSelectFields :: Alias -> [CoercibleSelectField] -> ([CoercibleSelectField], [HoistedAgg])
hoistFromSelectFields relAggAlias fields =
    let (newFields, maybeAggs) = foldr processField ([], []) fields
    in (newFields, catMaybes maybeAggs)
  where
    processField field (newFields, aggList) =
      let (modifiedField, maybeAgg) = modifyField field
      in (modifiedField : newFields, maybeAgg : aggList)

    modifyField field@CoercibleSelectField{csAggFunction=Just aggFunc, csField, csAggCast, csAlias} =
      let determineFieldName = fromMaybe (cfName csField) csAlias
          updatedField = field {csAggFunction = Nothing, csAggCast = Nothing}
          hoistedField = Just ((relAggAlias, determineFieldName), (aggFunc, csAggCast, csAlias))
      in (updatedField, hoistedField)
    modifyField field = (field, Nothing)

-- Taking the hoisted aggregates, modify the rel selects to apply the aggregates,
-- and any applicable casts or aliases.
hoistIntoRelSelectFields :: [HoistedAgg] -> RelSelectField -> RelSelectField
hoistIntoRelSelectFields aggList r@(Spread {rsSpreadSel = spreadSelects, rsAggAlias = relAggAlias}) =
    r { rsSpreadSel = map updateSelect spreadSelects }
  where
    updateSelect s =
        case lookup (relAggAlias, ssSelName s) aggList of
            Just (aggFunc, aggCast, fldAlias) ->
                s { ssSelAggFunction = Just aggFunc,
                    ssSelAggCast     = aggCast,
                    ssSelAlias       = fldAlias }
            Nothing -> s
hoistIntoRelSelectFields _ r = r

validateAggFunctions :: Bool -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
validateAggFunctions aggFunctionsAllowed (Node rp@ReadPlan {select} forest)
  | not aggFunctionsAllowed && any (isJust . csAggFunction) select = Left AggregatesNotAllowed
  | otherwise = Node rp <$> traverse (validateAggFunctions aggFunctionsAllowed) forest

-- | Lookup table in the schema cache before creating read plan
findTable :: QualifiedIdentifier -> TablesMap -> Either ApiRequestError QualifiedIdentifier
findTable qi@QualifiedIdentifier{..} tableMap =
  case HM.lookup qi tableMap of
    Nothing -> Left (TableNotFound qiSchema qiName (HM.elems tableMap))
    Just _ -> Right qi

addFilters :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addFilters ctx ApiRequest{..} rReq =
  foldr addFilterToNode (Right rReq) flts
  where
    QueryParams.QueryParams{..} = iQueryParams
    flts =
      case iAction of
        ActDb (ActRelationRead _  _) -> qsFilters
        ActDb (ActRoutine _ _)       -> qsFilters
        _                            -> qsFiltersNotRoot

    addFilterToNode :: (EmbedPath, Filter) -> Either ApiRequestError ReadPlanTree ->  Either ApiRequestError ReadPlanTree
    addFilterToNode =
      updateNode (\flt (Node q@ReadPlan{from=fromTable, where_=lf} f) -> Node q{ReadPlan.where_=addFilterToLogicForest (resolveFilter ctx{qi=fromTable} flt) lf}  f)

addOrders :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addOrders ctx ApiRequest{..} rReq =
  case iAction of
    ActDb (ActRelationMut _ _) -> Right rReq
    _                          -> foldr addOrderToNode (Right rReq) qsOrder
  where
    QueryParams.QueryParams{..} = iQueryParams

    addOrderToNode :: (EmbedPath, [OrderTerm]) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addOrderToNode = updateNode (\o (Node q f) -> Node q{order=resolveOrder ctx <$> o} f)

resolveOrder :: ResolverContext -> OrderTerm -> CoercibleOrderTerm
resolveOrder _ (OrderRelationTerm a b c d) = CoercibleOrderRelationTerm a b c d
resolveOrder ctx (OrderTerm fld dir nulls) = CoercibleOrderTerm (resolveTypeOrUnknown ctx fld Nothing) dir nulls

-- Validates that the related resource on the order is an embedded resource,
-- e.g. if `clients` is inside the `select` in /projects?order=clients(id)&select=*,clients(*),
-- and if it's a to-one relationship, it adds the right alias to the OrderRelationTerm so the generated query can succeed.
addRelatedOrders :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRelatedOrders (Node rp@ReadPlan{order,from} forest) = do
  newOrder <- newRelOrder `traverse` order
  Node rp{order=newOrder} <$> addRelatedOrders `traverse` forest
  where
    newRelOrder cot@CoercibleOrderTerm{}                   = Right cot
    newRelOrder cot@CoercibleOrderRelationTerm{coRelation} =
      let foundRP = rootLabel <$> find (\(Node ReadPlan{relName, relAlias} _) -> coRelation == fromMaybe relName relAlias) forest in
      case foundRP of
        Just ReadPlan{relName,relAlias,relAggAlias,relToParent} ->
          let isToOne = relIsToOne <$> relToParent
              name    = fromMaybe relName relAlias in
          if isToOne == Just True
            then Right $ cot{coRelation=relAggAlias}
            else Left $ RelatedOrderNotToOne (qiName from) name
        Nothing ->
          Left $ NotEmbedded coRelation

-- | Searches for null filters on embeds, e.g. `projects=not.is.null` on `GET /clients?select=*,projects(*)&projects=not.is.null`
--
-- (It doesn't err but uses an Either ApiRequestError type so it can combine with the other functions that modify the read plan tree)
--
-- Setup:
--
-- >>> let nullOp    = OpExpr True (Is IsNull)
-- >>> let nonNullOp = OpExpr False (Is IsNull)
-- >>> let notEqOp   = OpExpr True (Op OpNotEqual "val")
-- >>> :{
-- -- this represents the `projects(*)` part on `/clients?select=*,projects(*)`
-- let
-- subForestPlan =
--   [
--     Node {
--       rootLabel = ReadPlan {
--         select = [], -- there will be fields at this stage but we just omit them for brevity
--         from = QualifiedIdentifier {qiSchema = "test", qiName = "projects"},
--         fromAlias = Just "projects_1", where_ = [], order = [], range_ = fullRange,
--         relName = "projects",
--         relToParent = Nothing,
--         relJoinConds = [],
--         relAlias = Nothing, relAggAlias = "clients_projects_1", relHint = Nothing, relJoinType = Nothing, relIsSpread = False, depth = 1,
--         relSelect = []
--       },
--       subForest = []
--     }
--   ]
-- :}
--
-- >>> :{
-- -- this represents the full URL `/clients?select=*,projects(*)&projects=not.is.null`, if subForst takes the above subForestPlan and nullOp
-- let
-- readPlanTree op subForst =
--   Node {
--     rootLabel = ReadPlan {
--       select = [], -- there will be fields at this stage but we just omit them for brevity
--       from = QualifiedIdentifier { qiSchema = "test", qiName = "clients"},
--       fromAlias = Nothing,
--       where_ = [
--         CoercibleStmnt (
--           CoercibleFilter {
--            field = CoercibleField {cfName = "projects", cfJsonPath = [], cfToJson=False, cfToTsVector = Nothing, cfIRType = "", cfTransform = Nothing, cfDefault = Nothing, cfFullRow = False},
--            opExpr = op
--           }
--         )
--       ],
--       order = [], range_ = fullRange, relName = "clients", relToParent = Nothing, relJoinConds = [], relAlias = Nothing, relAggAlias = "", relHint = Nothing,
--       relJoinType = Nothing, relIsSpread = False, depth = 0,
--       relSelect = []
--     },
--     subForest = subForst
--   }
-- :}
--
-- Don't do anything to the filter if there's no embedding (a subtree) on projects. Assume it's a normal filter.
--
-- >>> ReadPlan.where_ . rootLabel <$> addNullEmbedFilters (readPlanTree nullOp [])
-- Right [CoercibleStmnt (CoercibleFilter {field = CoercibleField {cfName = "projects", cfJsonPath = [], cfToJson = False, cfToTsVector = Nothing, cfIRType = "", cfTransform = Nothing, cfDefault = Nothing, cfFullRow = False}, opExpr = OpExpr True (Is IsNull)})]
--
-- If there's an embedding on projects, then change the filter to use the internal aggregate name (`clients_projects_1`) so the filter can succeed later.
--
-- >>> ReadPlan.where_ . rootLabel <$> addNullEmbedFilters (readPlanTree nullOp subForestPlan)
-- Right [CoercibleStmnt (CoercibleFilterNullEmbed True "clients_projects_1")]
--
-- >>> ReadPlan.where_ . rootLabel <$> addNullEmbedFilters (readPlanTree nonNullOp subForestPlan)
-- Right [CoercibleStmnt (CoercibleFilterNullEmbed False "clients_projects_1")]
addNullEmbedFilters :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addNullEmbedFilters (Node rp@ReadPlan{where_=curLogic} forest) = do
  let forestReadPlans = rootLabel <$> forest
  newLogic <- newNullFilters forestReadPlans `traverse` curLogic
  Node rp{ReadPlan.where_= newLogic} <$> (addNullEmbedFilters `traverse` forest)
  where
    newNullFilters :: [ReadPlan] -> CoercibleLogicTree -> Either ApiRequestError CoercibleLogicTree
    newNullFilters rPlans = \case
      (CoercibleExpr b lOp trees) ->
        CoercibleExpr b lOp <$> (newNullFilters rPlans `traverse` trees)
      flt@(CoercibleStmnt (CoercibleFilter (CoercibleField fld [] _ _ _ _ _ _) opExpr)) ->
        let foundRP = find (\ReadPlan{relName, relAlias} -> fld == fromMaybe relName relAlias) rPlans in
        case (foundRP, opExpr) of
          (Just ReadPlan{relAggAlias}, OpExpr b (Is IsNull)) -> Right $ CoercibleStmnt $ CoercibleFilterNullEmbed b relAggAlias
          _                                                   -> Right flt
      flt@(CoercibleStmnt _) ->
        Right flt

addRanges :: ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRanges ApiRequest{..} rReq =
  case iAction of
    ActDb (ActRelationMut _ _) -> Right rReq
    _                          -> foldr addRangeToNode (Right rReq) =<< ranges
  where
    ranges :: Either ApiRequestError [(EmbedPath, NonnegRange)]
    ranges = first QueryParamError $ QueryParams.pRequestRange `traverse` HM.toList iRange

    addRangeToNode :: (EmbedPath, NonnegRange) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addRangeToNode = updateNode (\r (Node q f) -> Node q{range_=r} f)

addLogicTrees :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addLogicTrees ctx ApiRequest{..} rReq =
  foldr addLogicTreeToNode (Right rReq) qsLogic
  where
    QueryParams.QueryParams{..} = iQueryParams

    addLogicTreeToNode :: (EmbedPath, LogicTree) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addLogicTreeToNode = updateNode (\t (Node q@ReadPlan{from=fromTable, where_=lf} f) -> Node q{ReadPlan.where_=resolveLogicTree ctx{qi=fromTable} t:lf} f)

resolveLogicTree :: ResolverContext -> LogicTree -> CoercibleLogicTree
resolveLogicTree ctx (Stmnt flt) = CoercibleStmnt $ resolveFilter ctx flt
resolveLogicTree ctx (Expr b op lts) = CoercibleExpr b op (map (resolveLogicTree ctx) lts)

resolveFilter :: ResolverContext -> Filter -> CoercibleFilter
resolveFilter ctx (Filter fld opExpr) = CoercibleFilter{field=resolveQueryInputField ctx fld opExpr, opExpr=opExpr}

-- Validates that spread embeds are only done on to-one relationships
validateSpreadEmbeds :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
validateSpreadEmbeds (Node rp@ReadPlan{relToParent=Nothing} forest) = Node rp <$> validateSpreadEmbeds `traverse` forest
validateSpreadEmbeds (Node rp@ReadPlan{relIsSpread,relToParent=Just rel,relName} forest) = do
  validRP <- if relIsSpread && not (relIsToOne rel)
    then Left $ SpreadNotToOne (qiName $ relTable rel) relName -- TODO using relTable is not entirely right because ReadPlan might have an alias, need to store the parent alias on ReadPlan
    else Right rp
  Node validRP <$> validateSpreadEmbeds `traverse` forest

-- Find a Node of the Tree and apply a function to it
updateNode :: (a -> ReadPlanTree -> ReadPlanTree) -> (EmbedPath, a) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
updateNode f ([], a) rr = f a <$> rr
updateNode _ _ (Left e) = Left e
updateNode f (targetNodeName:remainingPath, a) (Right (Node rootNode forest)) =
  case findNode of
    Nothing -> Left $ NotEmbedded targetNodeName
    Just target ->
      (\node -> Node rootNode $ node : delete target forest) <$>
      updateNode f (remainingPath, a) (Right target)
  where
    findNode :: Maybe ReadPlanTree
    findNode = find (\(Node ReadPlan{relName, relAlias} _) -> relName == targetNodeName || relAlias == Just targetNodeName) forest

mutatePlan :: Mutation -> QualifiedIdentifier -> ApiRequest -> SchemaCache -> ReadPlanTree -> Either Error MutatePlan
mutatePlan mutation qi ApiRequest{iPreferences=Preferences{..}, ..} SchemaCache{dbTables, dbRepresentations} readReq = mapLeft ApiRequestError $
  case mutation of
    MutationCreate ->
      mapRight (\typedColumns -> Insert qi typedColumns body ((,) <$> preferResolution <*> Just confCols) [] returnings pkCols applyDefaults) typedColumnsOrError
    MutationUpdate ->
      mapRight (\typedColumns -> Update qi typedColumns body combinedLogic returnings applyDefaults) typedColumnsOrError
    MutationSingleUpsert ->
        if null qsLogic &&
           qsFilterFields == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (OpQuant OpEqual Nothing _)) -> True
              _                                                   -> False) qsFiltersRoot
          then mapRight (\typedColumns -> Insert qi typedColumns body (Just (MergeDuplicates, pkCols)) combinedLogic returnings mempty False) typedColumnsOrError
        else
          Left InvalidFilters
    MutationDelete -> Right $ Delete qi combinedLogic returnings
  where
    ctx = ResolverContext dbTables dbRepresentations qi "json"
    confCols = fromMaybe pkCols qsOnConflict
    QueryParams.QueryParams{..} = iQueryParams
    returnings =
      if preferRepresentation == Just None || isNothing preferRepresentation
        then []
        else inferColsEmbedNeeds readReq pkCols
    tbl = HM.lookup qi dbTables
    pkCols = maybe mempty tablePKCols tbl
    logic = map (resolveLogicTree ctx . snd) qsLogic
    combinedLogic = foldr (addFilterToLogicForest . resolveFilter ctx) logic qsFiltersRoot
    body = payRaw <$> iPayload -- the body is assumed to be json at this stage(ApiRequest validates)
    applyDefaults = preferMissing == Just ApplyDefaults
    typedColumnsOrError = resolveOrError ctx tbl `traverse` S.toList iColumns

resolveOrError :: ResolverContext -> Maybe Table -> FieldName -> Either ApiRequestError CoercibleField
resolveOrError _ Nothing _ = Left NotFound -- TODO: control never reaches here since #3869, should be fixed when fixing #3906
resolveOrError ctx (Just table) field =
  case resolveTableFieldName table field Nothing of
    CoercibleField{cfIRType=""} -> Left $ ColumnNotFound (tableName table) field
    cf                          -> Right $ withJsonParse ctx cf

callPlan :: Routine -> ApiRequest -> S.Set FieldName -> CallArgs -> ReadPlanTree -> CallPlan
callPlan proc ApiRequest{} paramKeys args readReq = FunctionCall {
  funCQi = QualifiedIdentifier (pdSchema proc) (pdName proc)
, funCParams = callParams
, funCArgs = args
, funCScalar = funcReturnsScalar proc
, funCSetOfScalar = funcReturnsSetOfScalar proc
, funCRetCompositeAlias = funcReturnsCompositeAlias proc
, funCReturning = inferColsEmbedNeeds readReq []
}
  where
    specifiedParams = filter (\x -> ppName x `S.member` paramKeys)
    callParams = case pdParams proc of
      [prm] | ppName prm == mempty -> OnePosParam prm
            | otherwise            -> KeyParams $ specifiedParams [prm]
      prms  -> KeyParams $ specifiedParams prms

-- | Infers the columns needed for an embed to be successful after a mutation or a function call.
inferColsEmbedNeeds :: ReadPlanTree -> [FieldName] -> [FieldName]
inferColsEmbedNeeds (Node ReadPlan{select} forest) pkCols
  -- if * is part of the select, we must not add pk or fk columns manually -
  -- otherwise those would be selected and output twice
  | "*" `elem` fldNames = ["*"]
  | otherwise           = returnings
  where
    fldNames = cfName . csField <$> select
    -- Without fkCols, when a mutatePlan to
    -- /projects?select=name,clients(name) occurs, the RETURNING SQL part would
    -- be `RETURNING name`(see QueryBuilder).  This would make the embedding
    -- fail because the following JOIN would need the "client_id" column from
    -- projects.  So this adds the foreign key columns to ensure the embedding
    -- succeeds, result would be `RETURNING name, client_id`.
    fkCols = concat $ mapMaybe (\case
        Node ReadPlan{relToParent=Just Relationship{relCardinality=O2M _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just Relationship{relCardinality=M2O _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just Relationship{relCardinality=O2O _ cols _}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just Relationship{relCardinality=M2M Junction{junColsSource=cols}}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just ComputedRelationship{}} _ ->
          Nothing
        Node ReadPlan{relToParent=Nothing} _ ->
          Nothing
      ) forest
    hasComputedRel = isJust $ find (\case
      Node ReadPlan{relToParent=Just ComputedRelationship{}} _ -> True
      _                                                    -> False
      ) forest
    -- However if the "client_id" is present, e.g. mutatePlan to
    -- /projects?select=client_id,name,clients(name) we would get `RETURNING
    -- client_id, name, client_id` and then we would produce the "column
    -- reference \"client_id\" is ambiguous" error from PostgreSQL. So we
    -- deduplicate with Set: We are adding the primary key columns as well to
    -- make sure, that a proper location header can always be built for
    -- INSERT/POST
    returnings =
      if not hasComputedRel
        then S.toList . S.fromList $ fldNames ++ fkCols ++ pkCols
        else ["*"] -- on computed relationships we cannot know the required columns for an embedding to succeed, so we just return all

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: CoercibleFilter -> [CoercibleLogicTree] -> [CoercibleLogicTree]
addFilterToLogicForest flt lf = CoercibleStmnt flt : lf

-- | Do content negotiation. i.e. choose a media type based on the intersection of accepted/produced media types.
negotiateContent :: AppConfig -> ApiRequest -> QualifiedIdentifier -> [MediaType] -> MediaHandlerMap -> Bool -> Either ApiRequestError ResolvedHandler
negotiateContent conf ApiRequest{iAction=act, iPreferences=Preferences{preferRepresentation=rep}} identifier accepts produces defaultSelect =
  case (act, firstAcceptedPick) of
    (_, Nothing)                                             -> Left . MediaTypeError $ map MediaType.toMime accepts
    (ActDb (ActRelationMut _ _),              Just (x, mt)) -> Right (if rep == Just Full then x else NoAgg, mt)
    -- no need for an aggregate on HEAD https://github.com/PostgREST/postgrest/issues/2849
    -- TODO: despite no aggregate, these are responding with a Content-Type, which is not correct.
    (ActDb (ActRelationRead _ True),             Just (_, mt)) -> Right (NoAgg, mt)
    (ActDb (ActRoutine  _ (InvRead True)), Just (_, mt))             -> Right (NoAgg, mt)
    (_, Just (x, mt))                                        -> Right (x, mt)
  where
    firstAcceptedPick = listToMaybe $ mapMaybe matchMT accepts -- If there are multiple accepted media types, pick the first. This is usual in content negotiation.
    matchMT mt = case mt of
      -- all the vendored media types have special handling as they have media type parameters, they cannot be overridden
      m@(MTVndSingularJSON strip)                 -> Just (BuiltinAggSingleJson strip, m)
      m@MTVndArrayJSONStrip                       -> Just (BuiltinAggArrayJsonStrip, m)
      m@(MTVndPlan (MTVndSingularJSON strip) _ _) -> mtPlanToNothing $ Just (BuiltinAggSingleJson strip, m)
      m@(MTVndPlan MTVndArrayJSONStrip _ _)       -> mtPlanToNothing $ Just (BuiltinAggArrayJsonStrip, m)
      -- TODO the plan should have its own MediaHandler instead of relying on MediaType
      m@(MTVndPlan mType _ _)                     -> mtPlanToNothing $ (,) <$> (fst <$> lookupHandler mType) <*> pure m
      -- all the other media types can be overridden
      x                                           -> lookupHandler x
    mtPlanToNothing x = if configDbPlanEnabled conf then x else Nothing -- don't find anything if the plan media type is not allowed
    lookupHandler mt =
      when' defaultSelect (HM.lookup (RelId identifier, MTAny) produces) <|> -- lookup for identifier and `*/*`
      when' defaultSelect (HM.lookup (RelId identifier, mt) produces) <|>    -- lookup for identifier and a particular media type
      HM.lookup (RelAnyElement, mt) produces                                    -- lookup for anyelement and a particular media type
    when' :: Bool -> Maybe a -> Maybe a
    when' True (Just a) = Just a
    when' _ _           = Nothing
