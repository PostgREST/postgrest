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
  ( wrappedReadPlan
  , mutateReadPlan
  , callReadPlan
  , inspectPlan
  , WrappedReadPlan(..)
  , MutateReadPlan(..)
  , CallReadPlan(..)
  , InspectPlan(..)
  ) where

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashMap.Strict.InsOrd    as HMI
import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified PostgREST.SchemaCache.Routine as Routine

import Data.Either.Combinators (mapLeft, mapRight)
import Data.List               (delete)
import Data.Tree               (Tree (..))

import PostgREST.ApiRequest                  (Action (..),
                                              ApiRequest (..),
                                              InvokeMethod (..),
                                              Mutation (..),
                                              Payload (..))
import PostgREST.Config                      (AppConfig (..))
import PostgREST.Error                       (Error (..))
import PostgREST.MediaType                   (MTPlanFormat (..),
                                              MediaType (..))
import PostgREST.Query.SqlFragment           (sourceCTEName)
import PostgREST.RangeQuery                  (NonnegRange, allRange,
                                              convertToLimitZeroRange,
                                              restrictRange)
import PostgREST.SchemaCache                 (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers     (FieldName,
                                              QualifiedIdentifier (..),
                                              Schema)
import PostgREST.SchemaCache.Relationship    (Cardinality (..),
                                              Junction (..),
                                              Relationship (..),
                                              RelationshipsMap,
                                              relIsToOne)
import PostgREST.SchemaCache.Representations (DataRepresentation (..),
                                              RepresentationsMap)
import PostgREST.SchemaCache.Routine         (ResultAggregate (..),
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

data WrappedReadPlan = WrappedReadPlan {
  wrReadPlan :: ReadPlanTree
, wrTxMode   :: SQL.Mode
, wrResAgg   :: ResultAggregate
, wrMedia    :: MediaType
}

data MutateReadPlan = MutateReadPlan {
  mrReadPlan   :: ReadPlanTree
, mrMutatePlan :: MutatePlan
, mrTxMode     :: SQL.Mode
, mrResAgg     :: ResultAggregate
, mrMedia      :: MediaType
}

data CallReadPlan = CallReadPlan {
  crReadPlan :: ReadPlanTree
, crCallPlan :: CallPlan
, crTxMode   :: SQL.Mode
, crProc     :: Routine
, crResAgg   :: ResultAggregate
, crMedia    :: MediaType
}

data InspectPlan = InspectPlan {
  ipMedia  :: MediaType
, ipTxmode :: SQL.Mode
}

wrappedReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Either Error WrappedReadPlan
wrappedReadPlan  identifier conf sCache apiRequest@ApiRequest{iPreferences=Preferences{..},..} = do
  rPlan <- readPlan identifier conf sCache apiRequest
  mediaType <- mapLeft ApiRequestError $ negotiateContent conf iAction iAcceptMediaType
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestError $ InvalidPreferences invalidPrefs else Right ()
  return $ WrappedReadPlan rPlan SQL.Read (mediaToAggregate mediaType apiRequest) mediaType

mutateReadPlan :: Mutation -> ApiRequest -> QualifiedIdentifier -> AppConfig -> SchemaCache -> Either Error MutateReadPlan
mutateReadPlan  mutation apiRequest@ApiRequest{iPreferences=Preferences{..},..} identifier conf sCache = do
  rPlan <- readPlan identifier conf sCache apiRequest
  mPlan <- mutatePlan mutation identifier apiRequest sCache rPlan
  mediaType <- mapLeft ApiRequestError $ negotiateContent conf iAction iAcceptMediaType
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestError $ InvalidPreferences invalidPrefs else Right ()
  return $ MutateReadPlan rPlan mPlan SQL.Write (mediaToAggregate mediaType apiRequest) mediaType

callReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> InvokeMethod -> Either Error CallReadPlan
callReadPlan identifier conf sCache apiRequest@ApiRequest{iPreferences=Preferences{..},..} invMethod = do
  let paramKeys = case invMethod of
        InvGet  -> S.fromList $ fst <$> qsParams'
        InvHead -> S.fromList $ fst <$> qsParams'
        InvPost -> iColumns
  proc@Function{..} <- mapLeft ApiRequestError $
    findProc identifier paramKeys (preferParameters == Just SingleObject) (dbRoutines sCache) iContentMediaType (invMethod == InvPost)
  let relIdentifier = QualifiedIdentifier pdSchema (fromMaybe pdName $ Routine.funcTableName proc) -- done so a set returning function can embed other relations
  rPlan <- readPlan relIdentifier conf sCache apiRequest
  let args = case (invMethod, iContentMediaType) of
        (InvGet, _)             -> jsonRpcParams proc qsParams'
        (InvHead, _)            -> jsonRpcParams proc qsParams'
        (InvPost, MTUrlEncoded) -> maybe mempty (jsonRpcParams proc . payArray) iPayload
        (InvPost, _)            -> maybe mempty payRaw iPayload
      txMode = case (invMethod, pdVolatility) of
          (InvGet,  _)                 -> SQL.Read
          (InvHead, _)                 -> SQL.Read
          (InvPost, Routine.Stable)    -> SQL.Read
          (InvPost, Routine.Immutable) -> SQL.Read
          (InvPost, Routine.Volatile)  -> SQL.Write
      cPlan = callPlan proc apiRequest paramKeys args rPlan
  mediaType <- mapLeft ApiRequestError $ negotiateContent conf iAction iAcceptMediaType
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestError $ InvalidPreferences invalidPrefs else Right ()
  return $ CallReadPlan rPlan cPlan txMode proc (mediaToAggregate mediaType apiRequest) mediaType
  where
    qsParams' = QueryParams.qsParams iQueryParams

inspectPlan :: AppConfig -> ApiRequest -> Either Error InspectPlan
inspectPlan conf apiRequest = do
  mediaType <- mapLeft ApiRequestError $ negotiateContent conf (iAction apiRequest) (iAcceptMediaType apiRequest)
  return $ InspectPlan mediaType SQL.Read

{-|
  Search a pg proc by matching name and arguments keys to parameters. Since a function can be overloaded,
  the name is not enough to find it. An overloaded function can have a different volatility or even a different return type.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> RoutineMap -> MediaType -> Bool -> Either ApiRequestError Routine
findProc qi argumentsKeys paramsAsSingleObject allProcs contentMediaType isInvPost =
  case matchProc of
    ([], [])     -> Left $ NoRpc (qiSchema qi) (qiName qi) (S.toList argumentsKeys) paramsAsSingleObject contentMediaType isInvPost (HM.keys allProcs) lookupProcName
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
        firstType = (ppType <$> headMay params)
      in
      -- exceptional case for Prefer: params=single-object
      if paramsAsSingleObject
        then length params == 1 && (firstType == Just "json" || firstType == Just "jsonb")
      -- If the function has no parameters, the arguments keys must be empty as well
      else if null params
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

resolveColumnField :: Column -> CoercibleField
resolveColumnField col = CoercibleField (colName col) mempty False (colNominalType col) Nothing (colDefault col)

resolveTableFieldName :: Table -> FieldName -> CoercibleField
resolveTableFieldName table fieldName =
  fromMaybe (unknownField fieldName []) $ HMI.lookup fieldName (tableColumns table) >>=
    Just . resolveColumnField

-- | Resolve a type within the context based on the given field name and JSON path. Although there are situations where failure to resolve a field is considered an error (see `resolveOrError`), there are also situations where we allow it (RPC calls). If it should be an error and `resolveOrError` doesn't fit, ensure to check the `cfIRType` isn't empty.
resolveTypeOrUnknown :: ResolverContext -> Field -> CoercibleField
resolveTypeOrUnknown ResolverContext{..} (fn, jp) =
  case res of
    -- types that are already json/jsonb don't need to be converted with `to_jsonb` for using arrow operators `data->attr`
    -- this prevents indexes not applying https://github.com/PostgREST/postgrest/issues/2594
    cf@CoercibleField{cfIRType="json"}  -> cf{cfJsonPath=jp, cfToJson=False}
    cf@CoercibleField{cfIRType="jsonb"} -> cf{cfJsonPath=jp, cfToJson=False}
    -- other types will get converted `to_jsonb(col)->attr`, even unknown types
    cf                                  -> cf{cfJsonPath=jp, cfToJson=True}
  where
    res = fromMaybe (unknownField fn jp) $ HM.lookup qi tables >>=
          Just . flip resolveTableFieldName fn

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
resolveOutputField ctx field = withOutputFormat ctx $ resolveTypeOrUnknown ctx field

-- | Map the query string format of a value (text) into the intermediate representation type, if available.
resolveQueryInputField :: ResolverContext -> Field -> CoercibleField
resolveQueryInputField ctx field = withTextParse ctx $ resolveTypeOrUnknown ctx field

-- | Builds the ReadPlan tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Either Error ReadPlanTree
readPlan qi@QualifiedIdentifier{..} AppConfig{configDbMaxRows} SchemaCache{dbTables, dbRelationships, dbRepresentations} apiRequest  =
  let
    -- JSON output format hardcoded for now. In the future we might want to support other output mappings such as CSV.
    ctx = ResolverContext dbTables dbRepresentations qi "json"
  in
    mapLeft ApiRequestError $
    treeRestrictRange configDbMaxRows (iAction apiRequest) =<<
    addNullEmbedFilters =<<
    validateSpreadEmbeds =<<
    addRelatedOrders =<<
    addDataRepresentationAliases =<<
    expandStarsForDataRepresentations ctx =<<
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
    defReadPlan = ReadPlan [] (QualifiedIdentifier mempty mempty) Nothing [] [] allRange mempty Nothing [] Nothing mempty Nothing Nothing False rootDepth
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
          Node q{select=(resolveOutputField ctx{qi=from q} selField, selCast, selAlias):select q} rForest

-- | Preserve the original field name if data representation is used to coerce the value.
addDataRepresentationAliases :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addDataRepresentationAliases rPlanTree = Right $ fmap (\rPlan@ReadPlan{select=sel} -> rPlan{select=map aliasSelectItem sel}) rPlanTree
  where
    aliasSelectItem :: (CoercibleField, Maybe Cast, Maybe Alias) -> (CoercibleField, Maybe Cast, Maybe Alias)
    -- If there already is an alias, don't overwrite it.
    aliasSelectItem (fld@(CoercibleField{cfName=fieldName, cfTransform=(Just _)}), Nothing, Nothing) = (fld, Nothing, Just fieldName)
    aliasSelectItem fld = fld

knownColumnsInContext :: ResolverContext -> [Column]
knownColumnsInContext ResolverContext{..} =
  fromMaybe [] $ HM.lookup qi tables >>=
  Just . tableColumnsList

-- | Expand "select *" into explicit field names of the table, if necessary to apply data representations.
expandStarsForDataRepresentations :: ResolverContext -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
expandStarsForDataRepresentations ctx@ResolverContext{qi} rPlanTree = Right $ fmap expandStars rPlanTree
  where
    expandStars :: ReadPlan -> ReadPlan
    -- When the schema is "" and the table is the source CTE, we assume the true source table is given in the from
    -- alias and belongs to the request schema. See the bit in `addRels` with `newFrom = ...`.
    expandStars rPlan@ReadPlan{from=(QualifiedIdentifier "" "pgrst_source"), fromAlias=(Just tblAlias)} =
      expandStarsForTable ctx{qi=qi{qiName=tblAlias}} rPlan
    expandStars rPlan@ReadPlan{from=fromTable} =
      expandStarsForTable ctx{qi=fromTable} rPlan

expandStarsForTable :: ResolverContext -> ReadPlan -> ReadPlan
expandStarsForTable ctx@ResolverContext{representations, outputType} rplan@ReadPlan{select=selectItems} =
  -- If we have a '*' select AND the target table has at least one data representation, expand.
  if ("*" `elem` map (\(field, _, _) -> cfName field) selectItems) && any hasOutputRep knownColumns
    then rplan{select=concatMap (expandStarSelectItem knownColumns) selectItems}
    else rplan
  where
    knownColumns = knownColumnsInContext ctx

    hasOutputRep :: Column -> Bool
    hasOutputRep col = HM.member (colNominalType col, outputType) representations

    expandStarSelectItem :: [Column] -> (CoercibleField, Maybe Cast, Maybe Alias) -> [(CoercibleField, Maybe Cast, Maybe Alias)]
    expandStarSelectItem columns (CoercibleField{cfName="*", cfJsonPath=[]}, b, c) = map (\col -> (withOutputFormat ctx $ resolveColumnField col, b, c)) columns
    expandStarSelectItem _ selectItem = [selectItem]

-- | Enforces the `max-rows` config on the result
treeRestrictRange :: Maybe Integer -> Action -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
treeRestrictRange _ (ActionMutate _) request = Right request
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
          ActionMutate _ -> rPlan{from=newFrom, fromAlias=newAlias}
          ActionInvoke _ -> rPlan{from=newFrom, fromAlias=newAlias}
          _              -> rPlan
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
    O2O _ cols ->
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
      O2M _ [(col, _)] -> hint_ == col
      M2O _ [(col, _)] -> hint_ == col
      O2O _ [(col, _)] -> hint_ == col
      _                -> False
    matchFKRefSingleCol hint_ card  = case card of
      O2M _ [(_, fCol)] -> hint_ == fCol
      M2O _ [(_, fCol)] -> hint_ == fCol
      O2O _ [(_, fCol)] -> hint_ == fCol
      _                 -> False
    matchConstraint tar card = case card of
      O2M cons _ -> tar == cons
      M2O cons _ -> tar == cons
      O2O cons _ -> tar == cons
      _          -> False
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

addFilters :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addFilters ctx ApiRequest{..} rReq =
  foldr addFilterToNode (Right rReq) flts
  where
    QueryParams.QueryParams{..} = iQueryParams
    flts =
      case iAction of
        ActionInvoke _ -> qsFilters
        ActionRead _   -> qsFilters
        _              -> qsFiltersNotRoot

    addFilterToNode :: (EmbedPath, Filter) -> Either ApiRequestError ReadPlanTree ->  Either ApiRequestError ReadPlanTree
    addFilterToNode =
      updateNode (\flt (Node q@ReadPlan{from=fromTable, where_=lf} f) -> Node q{ReadPlan.where_=addFilterToLogicForest (resolveFilter ctx{qi=fromTable} flt) lf}  f)

addOrders :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addOrders ctx ApiRequest{..} rReq =
  case iAction of
    ActionMutate _ -> Right rReq
    _              -> foldr addOrderToNode (Right rReq) qsOrder
  where
    QueryParams.QueryParams{..} = iQueryParams

    addOrderToNode :: (EmbedPath, [OrderTerm]) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addOrderToNode = updateNode (\o (Node q f) -> Node q{order=resolveOrder ctx <$> o} f)

resolveOrder :: ResolverContext -> OrderTerm -> CoercibleOrderTerm
resolveOrder _ (OrderRelationTerm a b c d) = CoercibleOrderRelationTerm a b c d
resolveOrder ctx (OrderTerm fld dir nulls) = CoercibleOrderTerm (resolveTypeOrUnknown ctx fld) dir nulls

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
-- >>> let nullOp    = OpExpr True (Is TriNull)
-- >>> let nonNullOp = OpExpr False (Is TriNull)
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
--         relAlias = Nothing, relAggAlias = "clients_projects_1", relHint = Nothing, relJoinType = Nothing, relIsSpread = False, depth = 1
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
--            field = CoercibleField {cfName = "projects", cfJsonPath = [], cfToJson=False, cfIRType = "", cfTransform = Nothing, cfDefault = Nothing},
--            opExpr = op
--           }
--         )
--       ],
--       order = [], range_ = fullRange, relName = "clients", relToParent = Nothing, relJoinConds = [], relAlias = Nothing, relAggAlias = "", relHint = Nothing,
--       relJoinType = Nothing, relIsSpread = False, depth = 0
--     },
--     subForest = subForst
--   }
-- :}
--
-- Don't do anything to the filter if there's no embedding (a subtree) on projects. Assume it's a normal filter.
--
-- >>> ReadPlan.where_ . rootLabel <$> addNullEmbedFilters (readPlanTree nullOp [])
-- Right [CoercibleStmnt (CoercibleFilter {field = CoercibleField {cfName = "projects", cfJsonPath = [], cfToJson = False, cfIRType = "", cfTransform = Nothing, cfDefault = Nothing}, opExpr = OpExpr True (Is TriNull)})]
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
      flt@(CoercibleStmnt (CoercibleFilter (CoercibleField fld [] _ _ _ _) opExpr)) ->
        let foundRP = find (\ReadPlan{relName, relAlias} -> fld == fromMaybe relName relAlias) rPlans in
        case (foundRP, opExpr) of
          (Just ReadPlan{relAggAlias}, OpExpr b (Is TriNull)) -> Right $ CoercibleStmnt $ CoercibleFilterNullEmbed b relAggAlias
          _                                                   -> Right flt
      flt@(CoercibleStmnt _) ->
        Right flt

addRanges :: ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRanges ApiRequest{..} rReq =
  case iAction of
    ActionMutate _ -> Right rReq
    _              -> foldr addRangeToNode (Right rReq) =<< ranges
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
resolveFilter ctx (Filter fld opExpr) = CoercibleFilter{field=resolveQueryInputField ctx fld, opExpr=opExpr}

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
      mapRight (\typedColumns -> Update qi typedColumns body combinedLogic iTopLevelRange rootOrder returnings applyDefaults) typedColumnsOrError
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
    MutationDelete -> Right $ Delete qi combinedLogic iTopLevelRange rootOrder returnings
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
    rootOrder = resolveOrder ctx <$> maybe [] snd (find (\(x, _) -> null x) qsOrder)
    combinedLogic = foldr (addFilterToLogicForest . resolveFilter ctx) logic qsFiltersRoot
    body = payRaw <$> iPayload -- the body is assumed to be json at this stage(ApiRequest validates)
    applyDefaults = preferMissing == Just ApplyDefaults
    typedColumnsOrError = resolveOrError ctx tbl `traverse` S.toList iColumns

resolveOrError :: ResolverContext -> Maybe Table -> FieldName -> Either ApiRequestError CoercibleField
resolveOrError _ Nothing _ = Left NotFound
resolveOrError ctx (Just table) field =
  case resolveTableFieldName table field of
    CoercibleField{cfIRType=""} -> Left $ ColumnNotFound (tableName table) field
    cf                          -> Right $ withJsonParse ctx cf

callPlan :: Routine -> ApiRequest -> S.Set FieldName -> LBS.ByteString -> ReadPlanTree -> CallPlan
callPlan proc ApiRequest{iPreferences=Preferences{..}} paramKeys args readReq = FunctionCall {
  funCQi = QualifiedIdentifier (pdSchema proc) (pdName proc)
, funCParams = callParams
, funCArgs = Just args
, funCScalar = funcReturnsScalar proc
, funCSetOfScalar = funcReturnsSetOfScalar proc
, funCRetCompositeAlias = funcReturnsCompositeAlias proc
, funCReturning = inferColsEmbedNeeds readReq []
}
  where
    paramsAsSingleObject = preferParameters == Just SingleObject
    specifiedParams = filter (\x -> ppName x `S.member` paramKeys)
    callParams = case pdParams proc of
      [prm] | paramsAsSingleObject -> OnePosParam prm
            | ppName prm == mempty -> OnePosParam prm
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
    fldNames = cfName . (\(f, _, _) -> f) <$> select
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
        Node ReadPlan{relToParent=Just Relationship{relCardinality=O2O _ cols}} _ ->
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

mediaToAggregate :: MediaType -> ApiRequest -> ResultAggregate
mediaToAggregate mt apiReq@ApiRequest{iAction=act, iPreferences=Preferences{preferRepresentation=rep}} =
  if noAgg then NoAgg
  else case mt of
    MTApplicationJSON     -> BuiltinAggJson
    MTSingularJSON strip  -> BuiltinAggSingleJson strip
    MTArrayJSONStrip      -> BuiltinAggArrayJsonStrip
    MTGeoJSON             -> BuiltinAggGeoJson
    MTTextCSV             -> BuiltinAggCsv
    MTAny                 -> BuiltinAggJson
    MTOpenAPI             -> BuiltinAggJson
    MTUrlEncoded          -> NoAgg -- TODO: unreachable since a previous step (producedMediaTypes) whitelists the media types that can become aggregates.

    -- Doing `Accept: application/vnd.pgrst.plan; for="application/vnd.pgrst.plan"` doesn't make sense, so we just empty the body.
    -- TODO: fail instead to be more strict
    MTPlan (MTPlan{}) _ _ -> NoAgg
    MTPlan media      _ _ -> mediaToAggregate media apiReq
    _                     -> NoAgg
  where
    noAgg = case act of
      ActionMutate _         -> rep == Just HeadersOnly || rep == Just None || isNothing rep
      ActionRead _isHead     -> _isHead -- no need for an aggregate on HEAD https://github.com/PostgREST/postgrest/issues/2849
      ActionInvoke invMethod -> invMethod == InvHead
      _                      -> False

-- | Do content negotiation. i.e. choose a media type based on the intersection of accepted/produced media types.
negotiateContent :: AppConfig -> Action -> [MediaType] -> Either ApiRequestError MediaType
negotiateContent conf action accepts =
  case firstAcceptedPick of
    Just MTAny -> Right MTApplicationJSON -- by default(for */*) we respond with json
    Just mt    -> Right mt
    Nothing    -> Left . MediaTypeError $ map MediaType.toMime accepts
  where
    -- if there are multiple accepted media types, pick the first
    firstAcceptedPick = listToMaybe $ L.intersect accepts $ producedMediaTypes conf action

producedMediaTypes :: AppConfig -> Action -> [MediaType]
producedMediaTypes conf action =
  case action of
    ActionRead _    -> defaultMediaTypes
    ActionInvoke _  -> defaultMediaTypes
    ActionInfo      -> defaultMediaTypes
    ActionMutate _  -> defaultMediaTypes
    ActionInspect _ -> inspectMediaTypes
  where
    inspectMediaTypes = [MTOpenAPI, MTApplicationJSON, MTArrayJSONStrip, MTAny]
    defaultMediaTypes =
      [MTApplicationJSON, MTArrayJSONStrip, MTSingularJSON True, MTSingularJSON False, MTGeoJSON, MTTextCSV] ++
      [MTPlan MTApplicationJSON PlanText mempty | configDbPlanEnabled conf] ++ [MTAny]
