module Main where

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.String (String)
import Protolude
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr)
import Text.Pretty.Simple (pPrint)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import qualified Options.Applicative as OptParse

import PostgREST.Types as Types
import PostgREST.DbStructure as DbStructure


data Options
  = Options
      { printResult :: Bool
      }

main :: IO ()
main =
  run =<< OptParse.execParser info

run :: Options -> IO ()
run options =
  do
    line <- B.getContents

    case Aeson.eitherDecode line :: Either String DbStructure.RawDbStructure of
      Right structure ->
        if (printResult options) then
          pPrint $ DbStructure.parseDbStructure structure
        else
          return ()

      Left err ->
        do
          hPutStrLn stderr err
          exitWith (ExitFailure 1)

info :: OptParse.ParserInfo Options
info =
  OptParse.info parser $
    OptParse.progDesc "PostgREST DbStructure tool"

parser :: OptParse.Parser Options
parser =
  Options
    <$> OptParse.switch
      ( OptParse.long "print-result"
      <> OptParse.help "URI for the Postgres database connection"
      )
