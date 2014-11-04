{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

module PgQuery (
  getRows
, insert
, upsert
, addUser
, signInRole
, setRole
, resetRole
, checkPass
, pgFormatIdentifier
, pgFormatLiteral
, RangedResult(..)
, LoginAttempt(..)
, DbRole
) where

import Data.Text (Text, splitOn, intercalate, replace, takeWhile)
import Data.String.Conversions (cs)
import Data.Functor ( (<$>) )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat)
import qualified Data.Map as M

import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import Control.Monad (join)

import qualified RangeQuery as R
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import qualified Network.HTTP.Types.URI as Net

import Types (SqlRow(..), getRow, sqlRowColumns, sqlRowValues)
import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy, validatePassword)

-- }}}

data RangedResult = RangedResult {
  rrFrom  :: Int
, rrTo    :: Int
, rrTotal :: Int
, rrBody  :: BL.ByteString
} deriving (Show)

type QuotedSql = (Text, [SqlValue])
type Schema = String
type DbRole = BS.ByteString

data LoginAttempt =
    NoCredentials
  | MalformedAuth
  | LoginFailed
  | LoginSuccess DbRole
  deriving (Eq, Show)

getRows :: Schema -> String -> Net.Query -> Maybe R.NonnegRange -> Connection -> IO RangedResult
getRows schema table qq range conn = do
  query <- populateSql conn
    $ globalAndLimitedCounts schema table qq <>
      jsonArrayRows
      (selectStarClause schema table
        <> whereClause qq
        <> orderClause qq
        <> limitClause range)
  r <- quickQuery conn (cs query) []

  return $ case r of
           [[total, _, SqlNull]] -> RangedResult offset 0 (fromSql total) "[]"
           [[total, limited_total, json]] ->
            RangedResult offset (offset + fromSql limited_total - 1)
                         (fromSql total) (fromSql json)
           _ -> RangedResult 0 0 0 "[]"

  where
    offset = fromMaybe 0 $ R.offset <$> range


whereClause :: Net.Query -> QuotedSql
whereClause qs =
  if null qs then ("", []) else (" where ", []) <> conjunction

  where
    cols = [ col | col <- qs, fst col `notElem` ["order"] ]
    conjunction = mconcat $ L.intersperse (" and ", []) (map wherePred cols)


orderClause :: Net.Query -> QuotedSql
orderClause qs = do
  let order = fromMaybe "" $ join $ lookup "order" qs
      terms = mapMaybe parseOrderTerm $ splitOn "," $ cs order
      termPred = mconcat $ L.intersperse (", ", []) (map orderTermSql terms)

  if null terms
     then ("", [])
     else (" order by ", []) <> termPred

  where
    parseOrderTerm :: Text -> Maybe OrderTerm
    parseOrderTerm s =
      case splitOn "." s of
           [d,c] ->
             if d `elem` ["asc", "desc"]
                then Just $ OrderTerm d c
                else Nothing
           _ -> Nothing

    orderTermSql :: OrderTerm -> QuotedSql
    orderTermSql t =
      ("%I " <> otDirection t, [toSql $ otColumn t])


data OrderTerm = OrderTerm {
  otDirection :: Text
, otColumn :: Text
}


wherePred :: Net.QueryItem -> QuotedSql
wherePred (column, predicate) =
  ("%I " <> op <> "%L", map toSql [column, value])

  where
    opCode:rest = BS.split '.' $ fromMaybe "." predicate
    value = BS.intercalate "." rest
    op = case opCode of
              "eq"  -> "="
              "gt"  -> ">"
              "lt"  -> "<"
              "gte" -> ">="
              "lte" -> "<="
              "neq" -> "<>"
              _     -> "="

limitClause :: Maybe R.NonnegRange -> QuotedSql
limitClause range =
  (" LIMIT %s OFFSET %s ", [toSql limit, toSql offset])

  where
    limit  = fromMaybe "ALL" $ show <$> (R.limit =<< range)
    offset = fromMaybe 0     $ R.offset <$> range

globalAndLimitedCounts :: Schema -> String -> Net.Query -> QuotedSql
globalAndLimitedCounts schema table qq =
  (" select ", [])
  <> ("(select count(1) from %I.%I ", map toSql [schema, table])
  <> whereClause qq
  <> ("), count(t), ", [])

selectStarClause :: Schema -> String -> QuotedSql
selectStarClause schema table =
  (" select * from %I.%I ", map toSql [schema, table])

jsonArrayRows :: QuotedSql -> QuotedSql
jsonArrayRows q =
  ("array_to_json(array_agg(row_to_json(t))) from (", []) <> q <> (") t", [])

insert :: Schema -> Text -> SqlRow -> Connection -> IO (M.Map String SqlValue)
insert schema table row conn = do
  sql    <- populateSql conn $ insertClause schema table row
  stmt   <- prepare conn $ cs sql
  _      <- execute stmt $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return m

addUser :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Connection -> IO ()
addUser identity pass role conn = do
  Just hashed <- hashPasswordUsingPolicy fastBcryptHashingPolicy $ cs pass
  _ <- quickQuery conn
         "insert into dbapi.auth (id, pass, rolname) values (?, ?, ?)"
         $ map toSql [identity, hashed, role]
  return ()

signInRole :: BS.ByteString -> BS.ByteString -> Connection -> IO LoginAttempt
signInRole user pass conn = do
  u <- quickQuery conn "select pass, rolname from dbapi.auth where id = ?" [toSql user]
  return $ case u of
    [[hashed, role]] ->
      if checkPass (fromSql hashed) (cs pass)
         then LoginSuccess $ fromSql role
         else LoginFailed
    _ -> LoginFailed

checkPass :: BS.ByteString -> BS.ByteString -> Bool
checkPass = validatePassword

upsert :: Schema -> Text -> SqlRow -> Net.Query -> Connection -> IO (M.Map String SqlValue)
upsert schema table row qq conn = do
  sql    <- populateSql conn $ upsertClause schema table row qq
  stmt   <- prepare conn $ cs sql
  _      <- execute stmt $ join $ replicate 2 $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return m

placeholders :: Text -> SqlRow -> Text
placeholders symbol = intercalate ", " . map (const symbol) . getRow

insertClause :: Schema -> Text -> SqlRow -> QuotedSql
insertClause schema table (SqlRow []) =
  ("insert into %I.%I default values returning *", [toSql schema, toSql table])
insertClause schema table row =
  ("insert into %I.%I (" <> placeholders "%I" row <> ")",
  map toSql $ cs schema : table : sqlRowColumns row)
  <> (" values (" <> placeholders "?" row <> ") returning *", sqlRowValues row)


insertClauseViaSelect :: Schema -> Text -> SqlRow -> QuotedSql
insertClauseViaSelect schema table row =
    ("insert into %I.%I (" <> placeholders "%I" row <> ")",
     map toSql $ cs schema : table : sqlRowColumns row)
  <> (" select " <> placeholders "?" row, sqlRowValues row)

updateClause :: Schema -> Text -> SqlRow -> QuotedSql
updateClause schema table row =
    ("update %I.%I set (" <> placeholders "%I" row <> ")",
     map toSql $ cs schema : table : sqlRowColumns row)
  <> (" = (" <> placeholders "?" row <> ")", [])

upsertClause :: Schema -> Text -> SqlRow -> Net.Query -> QuotedSql
upsertClause schema table row qq =
  ("with upsert as (", []) <> updateClause schema table row
  <> whereClause qq
  <> (" returning *) ", []) <> insertClauseViaSelect schema table row
  <> (" where not exists (select * from upsert) returning *", [])

populateSql :: Connection -> QuotedSql -> IO Text
populateSql conn sql = do
  [[escaped]] <- quickQuery conn (cs q) (snd sql)
  return $ fromSql escaped

  where
    q = mconcat [ "select format('", fst sql, "', ", ph (snd sql), ")" ]

    ph :: [a] -> Text
    ph = intercalate ", " . map (const "?::varchar")

pgFormatIdentifier :: Text -> Text
pgFormatIdentifier x =
  let escaped = replace "\"" "\"\"" (trimNullChars x) in
  if escaped =~ danger
    then "\"" <> escaped <> "\""
    else escaped

  where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: Text

pgFormatLiteral :: Text -> Text
pgFormatLiteral x =
  let trimmed = trimNullChars x
      escaped = "'" <> replace "'" "''" trimmed <> "'"
      slashed = replace "\\" "\\\\" escaped in
  if escaped =~ ("\\\\" :: Text)
    then "E" <> slashed
    else slashed

trimNullChars :: Text -> Text
trimNullChars = Data.Text.takeWhile (/= '\x0')

setRole :: Connection -> DbRole -> IO ()
setRole conn role = runRaw conn $ "set role " <> cs role

resetRole :: Connection -> IO ()
resetRole conn = runRaw conn "reset role"
