-- {{{ Imports
module PgQuery (
  getRows
, insert
, update
, upsert
, addUser
, signInRole
, setRole
, resetRole
, checkPass
, pgFmtIdent
, pgFmtLit
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

type Schema = Text
type DbRole = BS.ByteString

data LoginAttempt =
    NoCredentials
  | MalformedAuth
  | LoginFailed
  | LoginSuccess DbRole
  deriving (Eq, Show)

getRows :: Schema -> Text -> Net.Query -> Maybe R.NonnegRange -> Connection -> IO RangedResult
getRows schema table qq range conn = do
  r <- quickQuery conn (cs query) []

  return $ case r of
           [[total, _, SqlNull]] -> RangedResult offset 0 (fromSql total) "[]"
           [[total, limited_total, json]] ->
            RangedResult offset (offset + fromSql limited_total - 1)
                         (fromSql total) (fromSql json)
           _ -> RangedResult 0 0 0 "[]"

  where
    offset = fromMaybe 0 $ R.offset <$> range
    query  = globalAndLimitedCounts schema table qq <> jsonArrayRows (
        selectStarClause schema table
        <> whereClause qq
        <> orderClause qq
        <> limitClause range)


whereClause :: Net.Query -> Text
whereClause qs =
  if null qs then "" else " where " <> conjunction

  where
    cols = [ col | col <- qs, fst col `notElem` ["order"] ]
    conjunction = mconcat $ L.intersperse " and " (map wherePred cols)


orderClause :: Net.Query -> Text
orderClause qs = do
  let order = fromMaybe "" $ join $ lookup "order" qs
      terms = mapMaybe parseOrderTerm $ splitOn "," $ cs order
      termPred = mconcat $ L.intersperse ", " (map orderTermSql terms)

  if null terms
     then ""
     else " order by " <> termPred

  where
    parseOrderTerm :: Text -> Maybe OrderTerm
    parseOrderTerm s =
      case splitOn "." s of
           [d,c] ->
             if d `elem` ["asc", "desc"]
                then Just $ OrderTerm d c
                else Nothing
           _ -> Nothing

    orderTermSql :: OrderTerm -> Text
    orderTermSql t = pgFmtIdent (otColumn t) <> " " <> otDirection t


data OrderTerm = OrderTerm {
  otDirection :: Text
, otColumn :: Text
}


wherePred :: Net.QueryItem -> Text
wherePred (column, predicate) =
  pgFmtIdent (cs column) <> " " <> op <> " " <> pgFmtLit (cs value)

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

limitClause :: Maybe R.NonnegRange -> Text
limitClause range =
  cs $ " LIMIT " <> limit <> " OFFSET " <> show offset <> " "

  where
    limit  = fromMaybe "ALL" $ show <$> (R.limit =<< range)
    offset = fromMaybe 0     $ R.offset <$> range

globalAndLimitedCounts :: Schema -> Text -> Net.Query -> Text
globalAndLimitedCounts schema table qq =
  " select "
  <> "(select count(1) from " <> pgFmtIdent schema <> "." <> pgFmtIdent table <> " "
  <> whereClause qq
  <> "), count(t), "

selectStarClause :: Schema -> Text -> Text
selectStarClause schema table =
  " select * from " <> pgFmtIdent schema <> "." <> pgFmtIdent table <> " "

jsonArrayRows :: Text -> Text
jsonArrayRows q =
  "array_to_json(array_agg(row_to_json(t))) from (" <> q <> ") t"

insert :: Schema -> Text -> SqlRow -> Connection -> IO (M.Map String SqlValue)
insert schema table row conn = do
  stmt   <- prepare conn $ cs sql
  _      <- execute stmt $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return m

  where sql = insertClause schema table row

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

upsert :: Schema -> Text -> SqlRow -> Net.Query -> Connection ->
          IO (M.Map String SqlValue)
upsert schema table row qq conn = do
  stmt   <- prepare conn $ cs $ upsertClause schema table row qq
  _      <- execute stmt $ join $ replicate 2 $ sqlRowValues row
  m <- fetchRowMap stmt
  return $ fromMaybe M.empty m

update :: Schema -> Text -> SqlRow -> Net.Query -> Connection ->
          IO (M.Map String SqlValue)
update schema table row qq conn = do
  stmt   <- prepare conn $ cs $ updateClause schema table row qq
  _      <- execute stmt $ sqlRowValues row
  m <- fetchRowMap stmt
  return $ fromMaybe M.empty m

placeholders :: Text -> SqlRow -> Text
placeholders symbol = intercalate ", " . map (const symbol) . getRow

insertClause :: Schema -> Text -> SqlRow -> Text
insertClause schema table (SqlRow []) =
  "insert into " <> pgFmtIdent schema <> "." <> pgFmtIdent table <> " default values returning *"
insertClause schema table row =
  "insert into " <> pgFmtIdent schema <> "." <> pgFmtIdent table <> " (" <>
    intercalate ", " (map pgFmtIdent (sqlRowColumns row))
  <> ") values (" <> placeholders "?" row <> ") returning *"

insertClauseViaSelect :: Schema -> Text -> SqlRow -> Text
insertClauseViaSelect schema table row =
    "insert into " <> pgFmtIdent schema <> "." <> pgFmtIdent table <> " (" <>
      intercalate ", " (map pgFmtIdent (sqlRowColumns row))
  <> ") select " <> placeholders "?" row

updateClause :: Schema -> Text -> SqlRow -> Net.Query -> Text
updateClause schema table row qq =
    "update " <> pgFmtIdent schema <> "." <> pgFmtIdent table <> " set (" <>
      intercalate ", " (map pgFmtIdent (sqlRowColumns row))
  <> ") = (" <> placeholders "?" row <> ")"
  <> whereClause qq

upsertClause :: Schema -> Text -> SqlRow -> Net.Query -> Text
upsertClause schema table row qq =
  "with upsert as (" <> updateClause schema table row qq
  <> " returning *) " <> insertClauseViaSelect schema table row
  <> " where not exists (select * from upsert) returning *"

pgFmtIdent :: Text -> Text
pgFmtIdent x =
  let escaped = replace "\"" "\"\"" (trimNullChars x) in
  if escaped =~ danger
    then "\"" <> escaped <> "\""
    else escaped

  where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: Text

pgFmtLit :: Text -> Text
pgFmtLit x =
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
