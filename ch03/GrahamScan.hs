-- file: ch03/GrahamScan.hs

import Data.List

data Point = Point Double Double
             deriving (Eq, Show)

data Direction = LeftTurn
               | RightTurn
               | Straight
                 deriving (Eq, Show)

cross :: Point -> Point -> Point -> Double
cross (Point x1 y1) (Point x2 y2) (Point x3 y3) = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)^2+(y1-y2)^2)

turn :: Point -> Point -> Point -> Direction
turn pvt a b
    | crossProduct >  0 = LeftTurn
    | crossProduct <  0 = RightTurn
    | crossProduct == 0 = Straight
    where
        crossProduct = cross pvt a b

compareByY :: Point -> Point -> Ordering
compareByY (Point x1 y1) (Point x2 y2)
    | y1 /= y2  = compare y1 y2
    | otherwise = compare x1 y1

compareByAngle :: Point -> Point -> Point -> Ordering
compareByAngle pvt a b
    | angle /= EQ = angle
    | otherwise   = distance
    where
        angle = compare 0 (cross pvt a b)
        distance = if firstSection pvt a
                   then compare (dist pvt a) (dist pvt b)
                   else compare (dist pvt b) (dist pvt a)
                   where
                        firstSection (Point x1 y1) (Point x2 y2) = (x1 <= x2) && (y1 <= y2)
{-
 -              y
 -              ^
 -              |
 -              | first section
 -              |
 -          ----+----------------> x
 -              |
 -              |
 -}

lowestY :: [Point] -> Point
lowestY = minimumBy compareByY

grahamScan :: [Point] -> [Point]
grahamScan ps = scan [pvt] sortedList
    where
        sortedList = (sortBy (compareByAngle pvt) ((delete pvt ps))) ++ [pvt]
        pvt        = lowestY ps
        scan preResult (a:b:left)
            | direction /= RightTurn = scan (a:preResult) (b:left)
            | otherwise              = scan (tail preResult) (prePoint:b:left)
            where direction = turn prePoint a b
                  prePoint  = head preResult
        scan preResult _    = preResult

tupleListToPoints :: [(Double,Double)] -> [Point]
tupleListToPoints xs = map (\(x, y) -> (Point x y)) xs

pointsToTupleList :: [Point] -> [(Double, Double)]
pointsToTupleList xs = map (\(Point x y) -> (x, y)) xs

