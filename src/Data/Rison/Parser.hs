{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Rison.Parser where

import           Control.Applicative ( (<|>) )
import           Data.Aeson ( Value(..) )
import qualified Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 ( Parser
                                                  , scientific )
import qualified Data.ByteString as BS ( cons )
import qualified Data.HashMap.Strict as H
import           Data.Monoid ( (<>) )
import           Data.Strings ( byteToChar )
import qualified Data.Text as T
import           Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Vector as V
import           Data.Word ( Word8 )

import           Debug.Trace ( trace )

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define CLOSE_BRACKET 41
#define COMMA 44
#define DOUBLE_QUOTE 34
#define SINGLE_QUOTE 39
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define OPEN_BRACKET 40
#define COLON 58
#define EXCLAMATION 33
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

rison :: Parser Value
rison = value

value :: Parser Value
value = object <|> array <|> rstring <|> number <|> boolean <|> nulll

object :: Parser Value
object = do
  m <- (A.word8 OPEN_BRACKET *> objectValues <* A.word8 CLOSE_BRACKET) <|>
       (A.word8 OPEN_CURLY *> objectValues <* A.word8 CLOSE_CURLY)
  return $ Object m

objectValues :: Parser (H.HashMap T.Text Value)
objectValues = do
  w <- A.peekWord8'
  if w == CLOSE_CURLY || w == CLOSE_BRACKET
    then return H.empty
    else loop H.empty
 where
  loop m0 = do
    ident <- identifier <* A.word8 COLON
    v <- value
    let !m = H.insert ident v m0
    ch <- A.peekWord8'
    if ch == COMMA
      then A.word8 COMMA *> loop m
      else return m

array :: Parser Value
array = do
  v <- (A.word8 OPEN_SQUARE *> arrayValues CLOSE_SQUARE <* A.word8 CLOSE_SQUARE) <|>
       (A.word8 EXCLAMATION *>
        A.word8 OPEN_BRACKET *> arrayValues CLOSE_BRACKET <* A.word8 CLOSE_BRACKET)
  return $ Array v

arrayValues :: Word8 -> Parser (V.Vector Value)
arrayValues endChar = do
  w <- A.peekWord8'
  if w == endChar
    then return V.empty
    else loop []
  where
    loop acc = do
      v <- value
      ch <- A.peekWord8'
      if ch == COMMA
        then A.word8 COMMA *> loop (v:acc)
        else return (V.reverse (V.fromList (v:acc)))

boolean :: Parser Value
boolean = true <|> false
  where
    true = do
      A.word8 EXCLAMATION
      A.word8 C_t
      return $ Bool True
    false = do
      A.word8 EXCLAMATION
      A.word8 C_f
      return $ Bool False

nulll :: Parser Value
nulll = do
  A.word8 EXCLAMATION
  A.word8 C_n
  return $ Null

rstring :: Parser Value
rstring = do
  let dq = DOUBLE_QUOTE
      sq = SINGLE_QUOTE
  s <- identifier <|>
       A.word8 dq *> rstring_ dq <* A.word8 dq <|>
       A.word8 sq *> rstring_ sq <* A.word8 sq
  return $ String s

rstring_ :: Word8 -> Parser T.Text
rstring_ endCh = loop ""
  where
    loop acc = do
      w <- A.peekWord8'
      if w == endCh
        then return acc
        else do
          ch <- if w == EXCLAMATION || w == BACKSLASH
                then A.word8 w *> A.anyWord8
                else A.anyWord8
          loop $ T.snoc acc $ byteToChar ch

number :: Parser Value
number = Number <$> scientific

identifier :: Parser T.Text
identifier = do
  ch1 <- A.satisfy $ A.inClass "a-zA-Z"
  rest <- A.takeWhile $ A.inClass "0-9a-zA-Z"
  return $ decodeUtf8 (ch1 `BS.cons` rest)
