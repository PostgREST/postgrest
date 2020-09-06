module PostgREST.MegaQuery (getDbStructure) where

import qualified Hasql.Transaction   as HT

import qualified PostgREST.Types as Types
import Protolude


getDbStructure :: [Types.Schema] -> Types.DbStructure -> HT.Transaction Types.DbStructure
getDbStructure _ baseDbStructure =
  return Types.DbStructure
    { Types.dbTables = Types.dbTables baseDbStructure
    , Types.dbColumns = Types.dbColumns baseDbStructure
    , Types.dbRelations = Types.dbRelations baseDbStructure
    , Types.dbPrimaryKeys = Types.dbPrimaryKeys baseDbStructure
    , Types.dbProcs = Types.dbProcs baseDbStructure
    , Types.pgVersion = Types.pgVersion baseDbStructure
    }
