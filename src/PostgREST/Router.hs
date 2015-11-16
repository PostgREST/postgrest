module PostgREST.Router where

import           Data.Text             (Text)
import qualified Data.Text             as T
import           PostgREST.Types
import           PostgREST.DbStructure (dbFindTable)

data Endpoint =
    RootE
  | TableE Table
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

route _ _ _ _ = Nothing
