{-# LANGUAGE OverloadedStrings #-}

module Data.Rison.Writer ( write ) where

import           Data.Aeson ( Value(..) )
import           Data.ByteString ( ByteString )
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
import           Data.Monoid ( (<>) )
import           Data.Scientific ( toRealFloat )
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8
                                    , decodeUtf8 )
import           Data.Text.Format ( Only(..)
                                  , format
                                  , shortest )
import           Data.Text.Internal.Builder ( toLazyText )
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

write :: Value -> ByteString
write Null = "!n"
write (Bool True) = "!t"
write (Bool False) = "!f"
write (Number n) = encodeUtf8 . LT.toStrict . toLazyText . shortest $ n

write (String s) = quot <> encodeUtf8 (T.foldr esc "" s) <> quot
  where
    esc c acc | c == '!' = "!!" <> acc
              | c == '\\' = "!\\" <> acc
              | otherwise = c `T.cons` acc

    quot = if T.null . T.filter escChars $ s
           then ""
           else "'"

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
