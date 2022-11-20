module PartOne
    ( findHazardousPoints
    ) where

import           Data.List                      ( concat
                                                , find
                                                , transpose
                                                )
import qualified Data.Map                      as Map
-- import qualified Data.Set                      as Set
import           Lib                            ( Line(..)
                                                , Point(..)
                                                , enumPoints
                                                , pointFromXY
                                                )

type PointMap = Map.Map Point Int

findHazardousPoints :: [Line] -> Int
findHazardousPoints lineSegments =
    let pointMap  = Map.empty
        allPoints = findAllLinesPoints lineSegments
        pointsMap = getMapFromPoints Map.empty allPoints
    in  length $ filter (>= 2) $ Map.elems pointsMap

findAllLinesPoints :: [Line] -> [Point]
findAllLinesPoints [] = []
findAllLinesPoints (hdLine@(Line p1@(Point x1 y1) p2@(Point x2 y2)) : xs) =
    if lineIsHorizontal hdLine
        then enumPoints p1 p2 ++ findAllLinesPoints xs
        else findAllLinesPoints xs
  where
    lineIsHorizontal (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

getMapFromPoints :: PointMap -> [Point] -> PointMap
getMapFromPoints map [] = map
getMapFromPoints map (hd : tl) =
    let previousPointVal = Map.lookup hd map
    in  getMapFromPoints (populateMap previousPointVal) tl
  where
    populateMap val = case val of
        Just intValue -> Map.insert hd (intValue + 1) map
        Nothing       -> Map.insert hd 1 map
