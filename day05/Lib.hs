module Lib
    ( lineFromXY
    , pointFromXY
    , Point(..)
    , Line(..)
    , enumPoints
    , find3DirEnumPoints
    ) where

data Point = Point !Int !Int
    deriving (Show, Eq, Ord)

data Line = Line !Point !Point
    deriving (Show, Eq, Ord)

lineFromXY :: (Int, Int) -> (Int, Int) -> Line
lineFromXY (x1, y1) (x2, y2) = Line (Point x1 y1) (Point x2 y2)
pointFromXY :: (Int, Int) -> Point
pointFromXY (x, y) = Point x y

enumPoints :: Point -> Point -> [Point]
enumPoints p1@(Point x1 y1) p2@(Point x2 y2)
    | p1 == p2 = [p2]
    | p1 < p2 = if x1 < x2
        then p1 : enumPoints (pointFromXY (x1 + 1, y1)) p2
        else p1 : enumPoints (pointFromXY (x1, y1 + 1)) p2
    | otherwise = if x1 > x2
        then p1 : enumPoints (pointFromXY (x1 - 1, y1)) p2
        else p1 : enumPoints (pointFromXY (x1, y1 - 1)) p2

findDiagonalEnum p1@(Point x1 y1) p2@(Point x2 y2)
    | p1 == p2  = [p2]
    | otherwise = p1 : findDiagonalEnum (pointFromXY (newX, newY)) p2
  where
    newX = if (x1 < x2) then x1 + 1 else x1 - 1
    newY = if (y1 < y2) then y1 + 1 else y1 - 1


find3DirEnumPoints :: Point -> Point -> [Point]
find3DirEnumPoints p1@(Point x1 y1) p2@(Point x2 y2) =
    if p1 `isOrthogonalTo` p2 then enumPoints p1 p2 else findDiagonalEnum p1 p2
    where isOrthogonalTo (Point x1 y1) (Point x2 y2) = x1 == x2 || y1 == y2


