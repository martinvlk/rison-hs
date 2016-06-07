# rison-hs
Haskell library for parsing and rendering [RISON](https://github.com/Nanonid/rison) strings.

Rison gets parsed into and serialized from Aeson [Value](http://hackage.haskell.org/package/aeson-0.11.2.0/docs/Data-Aeson-Types.html#t:Value) objects.

Implementation partly inspired by [Aeson](https://github.com/bos/aeson).

##Example

```
{-# LANGUAGE OverloadedStrings #-}
import Data.Rison

example :: Bool
example = let inputString = "(property:!(val1,val2,val3))"
              aesonValue = decode inputString
              outputString = encode aesonValue
          in inputString == outputString
```
