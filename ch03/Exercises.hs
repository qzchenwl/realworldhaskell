-- file: ch03/Exercises.hs

import Data.Ord
import Data.Function

length' :: (Num b) => [a] -> b
length' = foldr (\x -> (+) 1) 0

mean' xs = sum xs / length' xs

palindrome' :: [a] -> [a]
palindrome' xs = xs ++ reverse xs

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == reverse xs

sortByLength' :: [[a]] -> [[a]]
sortByLength' []    = []
sortByLength' (p : xs) = sortByLength' lesser ++ [p] ++ sortByLength' greater
    where
        lesser   = filter (gt p) xs
        greater  = filter (le p) xs
        gt       = (>) `on` length              -- there's better implementation, only need to transverse the shorter list
        le       = (<=) `on` length

sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
sortBy' cmp []       = []
sortBy' cmp (p : xs) = sortBy' cmp lesser ++ [p] ++ sortBy' cmp greater
    where
        lesser  = filter (\x -> cmp x p == LT) xs
        greater = filter (\x -> (cmp x p == GT) || cmp x p == EQ) xs

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
                deriving (Eq, Show)

turn' (ax, ay) (bx, by) (cx, cy)
        | crossProduct > 0  = Left'
        | crossProduct < 0  = Right'
        | crossProduct == 0 = Straight'
        where
            crossProduct = x1*y2 - x2*y1
            (x1, y1)     = (bx - ax, by - ay)
            (x2, y2)     = (cx - bx, cy - by)

turnList' (a:b:c:xs) = turn' a b c : turnList' (b:c:xs)
turnList' _          = []


-- sort points first by y, if equals sort by x
compareP' (x1, y1) (x2, y2) = if y1 /= y2 then compare y1 y2 else compare x1 x2

-- cosine of angle line (p1, p2) make with x-axis
cos' (x1, y1) (x2, y2) = (x2 - x1) / sqrt ((x2 - x1)^2 + (y2 - y1)^2)

compareAngle (x1, y1) (x2, y2) (x3, y3) = comparing (cos' (x1, y1)) (x3, y3) (x2, y2)

grahamScan list = scan [] sortedList ++ [head sortedList]
    where
        sortedList = head list : sortBy' (compareAngle lowestP) (tail list)
        lowestP    = head (sortBy' compareP' list)
        scan preResult (p1:p2:p3:leftResult)
            | turn' p1 p2 p3 /= Right' = scan (p1:preResult) (p2:p3:leftResult)
            | otherwise                = scan (tail preResult) (head preResult:p2:p3:leftResult)
        scan preResult ps              = []
