{-# LANGUAGE OverloadedStrings #-}

module PostgREST.OpenAPI (
  encodeOpenAPI
  ) where

import           Control.Lens
import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Lazy        (ByteString)
import           Data.HashMap.Strict.InsOrd  (InsOrdHashMap, fromList)
import           Data.String                 (IsString (..))
import           Data.Text                   (Text, unpack, pack, concat, intercalate)
import qualified Data.Set                    as Set

import           Prelude hiding              (concat)

import           Data.Swagger

import           PostgREST.ApiRequest        (ContentType(..))
import           PostgREST.Config            (prettyVersion)
import           PostgREST.QueryBuilder      (operators)
import           PostgREST.Types             (Table(..), Column(..))

makeMimeList :: [ContentType] -> MimeList
makeMimeList cs = MimeList $ map (fromString . show) cs

toSwaggerType :: Text -> SwaggerType t
toSwaggerType "text"      = SwaggerString
toSwaggerType "integer"   = SwaggerInteger
toSwaggerType "boolean"   = SwaggerBoolean
toSwaggerType "numeric"   = SwaggerNumber
toSwaggerType _           = SwaggerString

makeProperty :: Column -> (Text, Referenced Schema)
makeProperty c = (colName c, Inline u)
  where
    r = mempty :: Schema
    s = if null $ colEnum c
           then r
           else r & enum_ .~ decode (encode (colEnum c))
    t = s & type_ .~ toSwaggerType (colType c)
    u = t & format ?~ colType c

makeProperties :: [Column] -> InsOrdHashMap Text (Referenced Schema)
makeProperties cs =  fromList $ map makeProperty cs

makeDefinition :: (Table, [Column], [Text]) -> (Text, Schema)
makeDefinition (t, cs, _) =
  let tn = tableName t in
      (tn, (mempty :: Schema)
        & type_ .~ SwaggerObject
        & properties .~ makeProperties cs)

makeDefinitions :: [(Table, [Column], [Text])] -> InsOrdHashMap Text Schema
makeDefinitions ti = fromList $ map makeDefinition ti

makeOperatorPattern :: Text
makeOperatorPattern =
  intercalate "|"
  [ concat ["^", x, y, "[.]"] |
    x <- ["not[.]", ""],
    y <- map fst operators ]

makeRowFilter :: Column -> Param
makeRowFilter c =
  (mempty :: Param)
  & name .~ colName c
  & required ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamQuery
    & type_ .~ SwaggerString
    & format ?~ colType c
    & pattern ?~ makeOperatorPattern)

makeRowFilters :: [Column] -> [Param]
makeRowFilters = map makeRowFilter

makeOrderItems :: [Column] -> [Text]
makeOrderItems cs =
  [ concat [x, y, z] |
    x <- map colName cs,
    y <- [".asc", ".desc", ""],
    z <- [".nullsfirst", ".nulllast", ""]
  ]

makeRangeParams :: [Param]
makeRangeParams =
  [ (mempty :: Param)
    & name        .~ "Range"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamHeader
      & type_ .~ SwaggerString)
  , (mempty :: Param)
    & name        .~ "Range-Unit"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamHeader
      & type_ .~ SwaggerString
      & default_ .~ decode "\"items\"")
  , (mempty :: Param)
    & name        .~ "offset"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString)
  , (mempty :: Param)
    & name        .~ "limit"
    & description ?~ "Limiting and Pagination"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString)
  ]

makePreferParam :: [Text] -> Param
makePreferParam ts =
  (mempty :: Param)
  & name        .~ "Prefer"
  & description ?~ "Preference"
  & required    ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamHeader
    & type_ .~ SwaggerString
    & enum_ .~ decode (encode ts))

makeGetParams :: [Column] -> [Param]
makeGetParams cs =
  makeRangeParams ++
  [ (mempty :: Param)
    & name        .~ "select"
    & description ?~ "Filtering Columns"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString)
  , (mempty :: Param)
    & name        .~ "order"
    & description ?~ "Ordering"
    & required    ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ .~ SwaggerString
      & enum_ .~ decode (encode $ makeOrderItems cs))
  , makePreferParam ["plurality=singular", "count=none"]
  ]

makeReturnPreferenceParam :: Param
makeReturnPreferenceParam =
  makePreferParam ["return=representation", "return=minimal", "return=none"]

makePostParams :: Text -> [Param]
makePostParams tn =
  [ makeReturnPreferenceParam
  , (mempty :: Param)
    & name        .~ "body"
    & description ?~ tn
    & required    ?~ False
    & schema .~ ParamBody (Ref (Reference tn))
  ]

makeDeleteParams :: [Param]
makeDeleteParams =
  [ makeReturnPreferenceParam ]

makePathItem :: (Table, [Column], [Text]) -> (FilePath, PathItem)
makePathItem (t, cs, _) = ("/" ++ unpack tn, p $ tableInsertable t)
  where
    tOp = (mempty :: Operation)
      & tags .~ Set.fromList [tn]
      & produces ?~ makeMimeList [ApplicationJSON, TextCSV]
      & at 200 ?~ "OK"
    getOp = tOp
      & parameters .~ map Inline (makeGetParams cs ++ rs)
      & at 206 ?~ "Partial Content"
    postOp = tOp
      & consumes ?~ makeMimeList [ApplicationJSON, TextCSV]
      & parameters .~ map Inline (makePostParams tn)
      & at 201 ?~ "Created"
    patchOp = tOp
      & consumes ?~ makeMimeList [ApplicationJSON, TextCSV]
      & parameters .~ map Inline (makePostParams tn ++ rs)
      & at 204 ?~ "No Content"
    deletOp = tOp
      & parameters .~ map Inline (makeDeleteParams ++ rs)
    pr = (mempty :: PathItem) & get ?~ getOp
    pw = pr & post ?~ postOp & patch ?~ patchOp & delete ?~ deletOp
    p False = pr
    p True  = pw
    rs = makeRowFilters cs
    tn = tableName t

makeRootPathItem :: (FilePath, PathItem)
makeRootPathItem = ("/", p)
  where
    getOp = (mempty :: Operation)
      & tags .~ Set.fromList ["/"]
      & produces ?~ makeMimeList [ApplicationJSON, OpenAPI]
      & at 200 ?~ "OK"
    pr = (mempty :: PathItem) & get ?~ getOp
    p = pr

makePathItems :: [(Table, [Column], [Text])] -> InsOrdHashMap FilePath PathItem
makePathItems ti = fromList $ makeRootPathItem : map makePathItem ti

postgrestSpec:: [(Table, [Column], [Text])] -> String -> Integer -> Swagger
postgrestSpec ti h p = (mempty :: Swagger)
  & basePath ?~ "/"
  & schemes ?~ [Http]
  & info .~ ((mempty :: Info)
      & version .~ pack prettyVersion
      & title .~ "PostgREST API"
      & description ?~ "This is a dynamic API generated by PostgREST")
  & host .~ h'
  & definitions .~ makeDefinitions ti
  & paths .~ makePathItems ti
    where
      h' = Just $ Host h (Just (fromInteger p))

encodeOpenAPI :: [(Table, [Column], [Text])] -> String -> Integer -> ByteString
encodeOpenAPI ti h p = encode $ postgrestSpec ti h p
