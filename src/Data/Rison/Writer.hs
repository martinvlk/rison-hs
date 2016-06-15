{-# LANGUAGE OverloadedStrings #-}

module Data.Rison.Writer ( write ) where

import           Data.Aeson ( Value(..) )
import           Data.ByteString ( ByteString )
import           Data.ByteString.Lazy ( toStrict )
import           Data.ByteString.Builder (toLazyByteString, integerDec)
import           Data.ByteString.Builder.Scientific (scientificBuilder)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
import           Data.Monoid ( (<>) )
import           Data.Scientific (base10Exponent, coefficient)
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8
                                    , decodeUtf8 )
import qualified Data.Vector as V

write :: Value -> ByteString
write Null = "!n"
write (Bool True) = "!t"
write (Bool False) = "!f"
write (Number n) = toStrict $ toLazyByteString numberbuilder
  where
    e = base10Exponent n
    numberbuilder
      | e < 0     = scientificBuilder n
      | otherwise = integerDec (coefficient n * 10 ^ e)


write (String s)
  | T.any escChars s = "'" <> encodedS <> "'"
  | otherwise = encodedS
  where
    encodedS = encodeUtf8 (T.concatMap esc s)
    esc c | c == '!' = "!!"
          | c == '\\' = "!\\"
          | otherwise = T.singleton c

    escChars '\\' = True
    escChars '!'  = True
    escChars  _   = False

write (Object m) = encodeUtf8 $
                   "(" <> T.intercalate "," (fmap pair sortedKeys) <> ")"
  where
    sortedKeys = L.sort $ H.keys m
    pair k = k <> ":" <> (decodeUtf8 . write . fromMaybe "" . H.lookup k $ m)

write (Array v) = encodeUtf8 $
                  "!(" <> T.intercalate ","
                  (decodeUtf8 . write <$> V.toList v) <> ")"
