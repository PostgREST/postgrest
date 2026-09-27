module Hasql.TestingKit.Constants where

import Hasql.Connection.Setting qualified as Setting

localConnectionSettings :: [Setting.Setting]
localConnectionSettings =
  [Setting.connection "postgresql://postgres:postgres@localhost:5432/postgres"]
