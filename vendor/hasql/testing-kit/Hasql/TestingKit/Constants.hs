module Hasql.TestingKit.Constants where

import qualified Hasql.Connection.Setting                  as Setting
import qualified Hasql.Connection.Setting.Connection       as Setting.Connection
import qualified Hasql.Connection.Setting.Connection.Param as Setting.Connection.Component

localConnectionSettings :: [Setting.Setting]
localConnectionSettings =
  [ Setting.connection
      ( Setting.Connection.params
          [ Setting.Connection.Component.host "localhost",
            Setting.Connection.Component.port 5432,
            Setting.Connection.Component.user "postgres",
            Setting.Connection.Component.password "postgres",
            Setting.Connection.Component.dbname "postgres"
          ]
      )
  ]
