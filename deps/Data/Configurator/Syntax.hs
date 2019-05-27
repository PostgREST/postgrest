{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Configurator.Syntax
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A parser for configuration files.

module Data.Configurator.Syntax
    (
      topLevel
    , interp
    ) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad (when)
import Data.Attoparsec.Text as A
import Data.Bits (shiftL)
import Data.Char (chr, isAlpha, isAlphaNum, isSpace)
import Data.Configurator.Types.Internal
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText, singleton, toLazyText)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

topLevel :: Parser [Directive]
topLevel = directives <* skipLWS <* endOfInput

directive :: Parser Directive
directive =
  mconcat [
    string "import" *> skipLWS *> (Import <$> string_)
  , string "#;" *> skipHWS *> (DirectiveComment <$> directive)
  , Bind <$> try (ident <* skipLWS <* char '=' <* skipLWS) <*> value
  , Group <$> try (ident <* skipLWS <* char '{' <* skipLWS)
          <*> directives <* skipLWS <* char '}'
  ]

directives :: Parser [Directive]
directives = (skipLWS *> directive <* skipHWS) `sepBy`
             (satisfy $ \c -> c == '\r' || c == '\n')

data Skip = Space | Comment

-- | Skip lines, comments, or horizontal white space.
skipLWS :: Parser ()
skipLWS = loop
  where
    loop = A.takeWhile isSpace >> ((comment >> loop) <|> return ())

    comment = try beginComment >> A.takeWhile (\c -> c /= '\r' && c /= '\n')

    beginComment = do
      _ <- A.char '#'
      mc <- peekChar
      case mc of
        Just ';' -> fail ""
        _ -> return ()

-- | Skip comments or horizontal white space.
skipHWS :: Parser ()
skipHWS = scan Space go *> pure ()
  where go Space ' '           = Just Space
        go Space '\t'          = Just Space
        go Space '#'           = Just Comment
        go Space _             = Nothing
        go Comment '\r'        = Nothing
        go Comment '\n'        = Nothing
        go Comment _           = Just Comment

data IdentState = First | Follow

ident :: Parser Name
ident = do
  n <- scan First go
  when (n == "import") $
    throw (ParseError "" $ "reserved word (" ++ show n ++ ") used as identifier")
  when (T.null n) $ fail "no identifier found"
  when (T.last n == '.') $ fail "identifier must not end with a dot"
  return n
 where
  go First c =
      if isAlpha c
      then Just Follow
      else Nothing
  go Follow c =
      if isAlphaNum c || c == '_' || c == '-'
      then Just Follow
      else if c == '.'
           then Just First
           else Nothing

value :: Parser Value
value = mconcat [
          string "on" *> pure (Bool True)
        , string "off" *> pure (Bool False)
        , string "true" *> pure (Bool True)
        , string "false" *> pure (Bool False)
        , String <$> string_
        , Number <$> scientific
        , List <$> brackets '[' ']'
                   ((value <* skipLWS) `sepBy` (char ',' <* skipLWS))
        ]

string_ :: Parser Text
string_ = do
  s <- char '"' *> scan False isChar <* char '"'
  if "\\" `T.isInfixOf` s
    then unescape s
    else return s
 where
  isChar True _ = Just False
  isChar _ '"'  = Nothing
  isChar _ c    = Just (c == '\\')

brackets :: Char -> Char -> Parser a -> Parser a
brackets open close p = char open *> skipLWS *> p <* char close

embed :: Parser a -> Text -> Parser a
embed p s = case parseOnly p s of
              Left err -> fail err
              Right v  -> return v

unescape :: Text -> Parser Text
unescape = fmap (L.toStrict . toLazyText) . embed (p mempty)
 where
  p acc = do
    h <- A.takeWhile (/='\\')
    let rest = do
          let cont c = p (acc `mappend` fromText h `mappend` singleton c)
          c <- char '\\' *> satisfy (inClass "ntru\"\\")
          case c of
            'n'  -> cont '\n'
            't'  -> cont '\t'
            'r'  -> cont '\r'
            '"'  -> cont '"'
            '\\' -> cont '\\'
            _    -> cont =<< hexQuad
    done <- atEnd
    if done
      then return (acc `mappend` fromText h)
      else rest

hexQuad :: Parser Char
hexQuad = do
  a <- embed hexadecimal =<< A.take 4
  if a < 0xd800 || a > 0xdfff
    then return (chr a)
    else do
      b <- embed hexadecimal =<< string "\\u" *> A.take 4
      if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
        then return $! chr (((a - 0xd800) `shiftL` 10) + (b - 0xdc00) + 0x10000)
        else fail "invalid UTF-16 surrogates"

-- | Parse a string interpolation spec.
--
-- The sequence @$$@ is treated as a single @$@ character.  The
-- sequence @$(@ begins a section to be interpolated, and @)@ ends it.
interp :: Parser [Interpolate]
interp = reverse <$> p []
 where
  p acc = do
    h <- Literal <$> A.takeWhile (/='$')
    let rest = do
          let cont x = p (x : h : acc)
          c <- char '$' *> satisfy (\c -> c == '$' || c == '(')
          case c of
            '$' -> cont (Literal (T.singleton '$'))
            _   -> (cont . Interpolate) =<< A.takeWhile1 (/=')') <* char ')'
    done <- atEnd
    if done
      then return (h : acc)
      else rest
