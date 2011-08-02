-- file: ch10/TypeConstraint.hs

data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                             deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
    | a < b     = isIncreasing rest
    | otherwise = False
isIncreasing _ = True

push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s
