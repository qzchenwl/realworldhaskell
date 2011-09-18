-- file: ch03/GrahamScan.hs

import Data.List
import Data.Ord

type Point = (Double, Double)

data Direction = LeftTurn
               | RightTurn
               | Straight
                 deriving (Eq, Show)

cross :: Point -> Point -> Point -> Double
cross (x1, y1) (x2, y2) (x3, y3) = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt((x1-x2)^2+(y1-y2)^2)

turn :: Point -> Point -> Point -> Direction
turn pvt a b
    | crossProduct >  0 = LeftTurn
    | crossProduct <  0 = RightTurn
    | crossProduct == 0 = Straight
    where
        crossProduct = cross pvt a b

compareByY :: Point -> Point -> Ordering
compareByY (x1, y1) (x2, y2)
    | y1 /= y2  = compare y1 y2
    | otherwise = compare x1 y1

compareByAngle :: Point -> Point -> Point -> Ordering
compareByAngle pvt a b
    | angle /= EQ = angle
    | otherwise   = distance
    where
        angle = compare 0 (cross pvt a b)
        distance = if firstQuadrant pvt a
                   then comparing (dist pvt) a b
                   else comparing (dist pvt) b a
                   where
                        firstQuadrant (x1, y1) (x2, y2) = (x1 <= x2) && (y1 <= y2)
{-
 -              y
 -              ^
 -              |
 -              | First Quadrant
 -              |
 -          ----+----------------> x
 -              |
 -              |
 -}

lowestY :: [Point] -> Point
lowestY = minimumBy compareByY

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan ps = scan [pvt] sortedList
    where
        sortedList = sortBy (compareByAngle pvt) (delete pvt ps) ++ [pvt]
        pvt        = lowestY ps
        scan preResult (a:b:left)
            | direction /= RightTurn = scan (a:preResult) (b:left)
            | otherwise              = scan (tail preResult) (prePoint:b:left)
            where direction = turn prePoint a b
                  prePoint  = head preResult
        scan preResult _    = preResult