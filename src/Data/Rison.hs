module Data.Rison (
    decode
  , encode
  ) where

import           Data.Aeson ( FromJSON
                            , ToJSON
                            , fromJSON
                            , toJSON
                            , Result(..) )
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString ( ByteString )
import           Data.Rison.Parser
import           Data.Rison.Writer

decode :: FromJSON a => ByteString -> Either String a
decode bs = do
  pr <- A.parseOnly rison bs
  case fromJSON pr of
    Error   e -> Left  e
    Success a -> Right a

encode :: ToJSON a => a -> ByteString
encode = write . toJSON
