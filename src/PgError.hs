{-# LANGUAGE OverloadedStrings #-}

module PgError (Message(..), parseMessage) where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Map as M
import Data.Text

import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI, mk)

data Message = Message {
    msgStatus :: Maybe Text
  , msgCode   :: Maybe Text
  , msgText   :: Maybe Text
  , msgHint   :: Maybe Text
} deriving (Show)

parseMessage :: Parser Message
parseMessage = do
  ps <- sepBy valPair (char ';' >> optional (char ' '))
  let m = M.fromList ps
  return $ Message
    (M.lookup "status" m)
    (M.lookup "code" m)
    (M.lookup "message" m)
    (M.lookup "hint" m)

valPair :: Parser (CI Text, Text)
valPair = do
  name <- many1 letter
  _ <- char ':'
  optional $ char ' '
  _ <- char '"'
  val <- many1 (noneOf "\"")
  _ <- char '"'
  return (mk (cs name), cs val)
