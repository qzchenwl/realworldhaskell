-- file: ch04/dlts.hs

import Data.List (isPrefixOf)

dlts :: String -> [String]
dlts = foldr step [] . lines
    where step line ds
            | "#define DLT_" `isPrefixOf` line = secondWord line : ds
            | otherwise                        = ds
          secondWord = head . tail . words
