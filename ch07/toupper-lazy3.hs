-- file: ch07/toupper-lazy3.hs

import Data.Char(toUpper)

main = do
    inpStr <- readFile "huge-input.txt"
    writeFile "huge-output.txt" (map toUpper inpStr)
