{-# LANGUAGE OverloadedStrings #-}

module PgError (Message(..), message, httpStatus) where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Map as M
import Text.Regex.TDFA.Text ()
import Data.Text hiding (drop, concat, head)
import Data.Aeson
import Data.Maybe
import Control.Monad (void)

import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI, mk)

import Network.HTTP.Types.Status

data Message = Message {
    msgStatus :: Maybe Text
  , msgCode   :: Text
  , msgText   :: Maybe Text
  , msgHint   :: Maybe Text
} deriving (Show, Eq)

message :: Parser Message
message = do
  ps <- sepBy valPair (char ';')
  let m = M.fromList ps
  return $ Message
    (M.lookup "status" m)
    (fromMaybe "" $ M.lookup "code" m)
    (M.lookup "message" m)
    (M.lookup "hint" m)

valPair :: Parser (CI Text, Text)
valPair = do
  _ <- spaces
  name <- many1 letter
  _ <- char ':'
  spaces
  _ <- many $ char '"'
  val <- manyTill anyChar $
    try
      (void $ many (char '"') >> (
        (void . lookAhead $ (char ';'))
        <|> ((optional $ char '.') >> eof)
      ))
  return (mk (cs name), cs val)


instance ToJSON Message where
  toJSON t = object [
      "message" .= msgText t
    , "code"    .= msgCode t
    , "status"  .= msgStatus t
    , "hint"    .= msgHint t
    ]

httpStatus :: Message -> Status
httpStatus m =
  let code = cs $ msgCode m :: String in
  case code of
    '0' : '8' : _ -> status503 -- pg connection err
    '0' : '9' : _ -> status500 -- triggered action exception
    '0' : 'L' : _ -> status403 -- invalid grantor
    '0' : 'P' : _ -> status403 -- invalid role specification
    '2' : '5' : _ -> status500 -- invalid tx state
    '2' : '8' : _ -> status403 -- invalid auth specification
    '2' : 'D' : _ -> status500 -- invalid tx termination
    '3' : '8' : _ -> status500 -- external routine exception
    '3' : '9' : _ -> status500 -- external routine invocation
    '3' : 'B' : _ -> status500 -- savepoint exception
    '4' : '0' : _ -> status500 -- tx rollback
    '5' : '3' : _ -> status503 -- insufficient resources
    '5' : '4' : _ -> status413 -- too complex
    '5' : '5' : _ -> status500 -- obj not on prereq state
    '5' : '7' : _ -> status500 -- operator intervention
    '5' : '8' : _ -> status500 -- system error
    'F' : '0' : _ -> status500 -- conf file error
    'H' : 'V' : _ -> status500 -- foreign data wrapper error
    'P' : '0' : _ -> status500 -- PL/pgSQL Error
    'X' : 'X' : _ -> status500 -- internal Error
    "42P01" -> status404 -- undefined table
    "42501" -> status404 -- insufficient privilege
    _ -> status400
