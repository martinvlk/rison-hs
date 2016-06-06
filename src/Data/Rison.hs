module Data.Rison (
  decode
  ) where

import           Data.Aeson ( Value(..) )
import qualified Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 ( Parser )
import           Data.ByteString ( ByteString )
import           Data.Rison.Parser

decode :: ByteString -> Either String Value
decode input = A.parseOnly rison input
