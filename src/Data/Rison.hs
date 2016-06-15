module Data.Rison (
    decode
  , encode
  ) where

import           Data.Aeson ( Value(..) )
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString ( ByteString )
import           Data.Rison.Parser
import           Data.Rison.Writer

decode :: ByteString -> Either String Value
decode = A.parseOnly rison

encode :: Value -> ByteString
encode = write
