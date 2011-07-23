-- file: ch03/Exercises.hs

length' :: (Num b) => [a] -> b
length' (x:xs) = 1 + length' xs
length' []     = 0

mean' xs = (sum xs) / (length' xs)

palindrome' :: [a] -> [a]
palindrome' xs = xs ++ reverse xs

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == reverse xs

sortByLength' :: [[a]] -> [[a]]
sortByLength' []    = []
sortByLength' (p : xs) = (sortByLength' lesser) ++ [p] ++ (sortByLength' greater)
    where
        lesser   = filter (lt p) xs
        greater  = filter (ge p) xs
        lt xs ys = length ys < length xs
        ge xs ys = length ys >= length xs

intersperse' :: a -> [[a]] -> [a]
intersperse' s (x:xs)
             | length xs > 0 = x ++ [s] ++ intersperse' s xs
             | otherwise     = x
intersperse' s []            = []


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height' (Node x l r) = 1 + max (height' l) (height' r)
height' Empty        = 0

data Direction' = Left'
                | Right'
                | Straight'
                deriving (Show)

turn' (ax, ay) (bx, by) (cx, cy)
        | crossProduct > 0  = Left'
        | crossProduct < 0  = Right'
        | crossProduct == 0 = Straight'
        where
            crossProduct = x1*y2 - x2*y1
            (x1, y1)     = (bx - ax, by - ay)
            (x2, y2)     = (cx - bx, cy - by)

turnList' (a:b:c:xs) = (turn' a b c) : turnList' (b:c:xs)
turnList' _          = []
