module Hasql.DynamicStatements.Session where

import qualified Hasql.Decoders                       as Decoders
import           Hasql.DynamicStatements.Prelude
import qualified Hasql.DynamicStatements.Snippet.Defs as SnippetDefs
import qualified Hasql.DynamicStatements.Statement    as Statement
import           Hasql.Session
import qualified Hasql.Session                        as Session

-- |
-- Execute a dynamically parameterized statement, providing a result decoder.
--
-- This is merely a shortcut, which immediately embeds
-- @Hasql.DynamicStatements.Statement.'Statement.dynamicallyParameterized'@
-- in @Session@.
-- For details see the docs on that function.
dynamicallyParameterizedStatement :: SnippetDefs.Snippet -> Decoders.Result result -> Bool -> Session result
dynamicallyParameterizedStatement snippet decoder prepared =
  Session.statement () (Statement.dynamicallyParameterized snippet decoder prepared)
