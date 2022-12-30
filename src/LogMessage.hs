{-# LANGUAGE OverloadedStrings #-}
module LogMessage where

import Fmt
import Data.Text(Text)

write :: Text -> Int -> Text
write txt i = fmt $ "A szoveg: " +| txt |+ ", es a szam: " +|| i ||+ "" +|| 3.5 ||+ ""


