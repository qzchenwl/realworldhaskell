-- file: ch11/Prettify2.hs

module Prettify2 where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)
   
empty :: Doc
empty = Empty

(<>)  :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y


prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

