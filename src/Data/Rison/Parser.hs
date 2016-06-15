{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Rison.Parser (
  rison
) where

import           Control.Applicative ( (<|>) )
import           Data.Aeson ( Value(..) )
import qualified Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 ( Parser
                                                  , scientific
                                                  , char )
import qualified Data.ByteString as BS ( cons )
import           Data.Char (chr)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Vector as V

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
        A.<?> "unsupported value type"

object :: Parser Value
object = do
  m <- (A.word8 OPEN_BRACKET *> objectValues <* A.word8 CLOSE_BRACKET)
       A.<?> "object"
  return $ Object m

objectValues :: Parser (H.HashMap T.Text Value)
objectValues = do
  w <- A.peekWord8'
  if w == CLOSE_BRACKET
    then return H.empty
    else loop H.empty
 where
  loop m0 = do
    ident <- (rstring' <|> identifier) <* char ':'
             A.<?> "object property identifier"
    v <- value
         A.<?> "object property value"
    let !m = H.insert ident v m0
    ch <- A.peekWord8'
    if ch == COMMA
      then A.word8 COMMA *> loop m
      else return m

array :: Parser Value
array = do
  v <- A.word8 EXCLAMATION *>
       A.word8 OPEN_BRACKET *> arrayValues <* A.word8 CLOSE_BRACKET
  return $ Array v

arrayValues :: Parser (V.Vector Value)
arrayValues = do
  w <- A.peekWord8'
  if w == CLOSE_BRACKET
    then return V.empty
    else loop []
  where
    loop acc = do
      v <- value A.<?> "array value"
      ch <- A.peekWord8'
      if ch == COMMA
        then A.word8 COMMA *> loop (v:acc)
        else return (V.reverse (V.fromList (v:acc)))

boolean :: Parser Value
boolean = do
    _ <- A.word8 EXCLAMATION
    true <|> false
  where
    true = Bool True <$ A.word8 C_t
    false = Bool False <$ A.word8 C_f

nulll :: Parser Value
nulll = do
  _ <- A.word8 EXCLAMATION
  _ <- A.word8 C_n
  return Null

rstring :: Parser Value
rstring = String <$> rstring'

rstring' :: Parser T.Text
rstring' = identifier <|> (A.word8 sq *> rstring_ "" <* A.word8 sq)
  where
    sq = SINGLE_QUOTE
    rstring_ acc = do
      w <- A.peekWord8'
      if w == sq
        then return acc
        else do
          ch <- if w == EXCLAMATION || w == BACKSLASH
                then A.word8 w *> A.anyWord8
                else A.anyWord8
          rstring_ $ T.snoc acc $ chr (fromIntegral ch)

number :: Parser Value
number = Number <$> scientific
         A.<?> "number"

identifier :: Parser T.Text
identifier = do
  ch1 <- A.satisfy $ A.inClass "a-zA-Z_"
  rest <- A.takeWhile $ A.inClass ".0-9a-zA-Z/_-"
  return $ decodeUtf8 (ch1 `BS.cons` rest)
