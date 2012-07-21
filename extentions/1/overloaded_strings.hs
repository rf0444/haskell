{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.ByteString.Char8 () -- for IsString ByteString

-- require extention of OverloadedStrings
hogebstr :: ByteString
hogebstr = "hoge"


main = return ()

