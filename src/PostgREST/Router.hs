module PostgREST.Router where

import           Data.Text             (Text)
import qualified Data.Text             as T
import           PostgREST.Types
import           PostgREST.Parsers     (pUriValues)
import           PostgREST.DbStructure (dbFindTable, dbFindPrimaryKeys)

data Endpoint =
    RootE
  | TableE Table
  | RowE Locus
  | ProcedureE Text

route :: Endpoint -> DbStructure -> Schema -> [Text] -> Maybe Endpoint
route lastE _ _ [] = Just lastE

route RootE _ _ ["rpc", proc] = Just $ ProcedureE proc

route RootE _ _ [proc] | T.head proc == '@' = Just $ ProcedureE (T.tail proc)

route RootE db schema (p:ps) =
  case maybeTable of
    Just table -> route (TableE table) db schema ps
    _ -> Nothing
  where
    maybeTable = dbFindTable db schema p

route (TableE table) db schema (p:ps) =
  case maybeLocus of
    Just locus -> route (RowE locus) db schema ps
    _ -> Nothing
  where
    pKeys = map pkName $ dbFindPrimaryKeys db table
    uriVals = pUriValues p
    vals = zipWith makeFilter pKeys uriVals
    makeFilter key val = Filter (key, Nothing) "=" (VText val)
    maybeLocus =
      if length uriVals == length pKeys
        then Just $ Locus table vals
        else Nothing

route _ _ _ _ = Nothing
