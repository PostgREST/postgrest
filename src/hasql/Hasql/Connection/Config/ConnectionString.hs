module Hasql.Connection.Config.ConnectionString where

import qualified Data.ByteString                                 as B
import qualified Data.ByteString.Char8                           as BC
import qualified Data.Map.Strict                                 as Map
import qualified Data.Text.Encoding
import qualified Hasql.Connection.Config.ConnectionString.Params as Params
import           Hasql.Prelude

type ConnectionString = ByteString

class Constructs a where
  construct :: a -> ConnectionString

fromText :: Text -> ConnectionString
fromText = Data.Text.Encoding.encodeUtf8

fromParams :: Params.Params -> ConnectionString
fromParams =
  B.intercalate " " . fmap renderParam . Map.toList
  where
    renderParam (k, v) = mconcat [k, "=", "'", escapeSingleQuote v, "'"]

    escapeSingleQuote =
      BC.concatMap
        ( \w ->
            case w of
              '\'' -> BC.pack "\\'"
              '\\' -> BC.pack "\\\\"
              _    -> BC.singleton w
        )
