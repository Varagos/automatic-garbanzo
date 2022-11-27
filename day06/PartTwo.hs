module PartTwo
    ( fishCountAfterDays
    ) where
import qualified Data.Map                      as Map


type LanternFishHistogram = Map.Map Int Int


listToFishHistogram :: [Int] -> LanternFishHistogram
listToFishHistogram l = Map.fromListWith (+) $ zip l (repeat 1)


-- All zeros become 6 (and produce new 8's), all the rest become - 1
nextDayHistogramList :: [(Int, Int)] -> [(Int, Int)]
nextDayHistogramList []                = []
nextDayHistogramList ((age, cnt) : xs) = case age `compare` 0 of
    GT -> (age - 1, cnt) : nextDayHistogramList xs
    EQ -> (6, cnt) : (8, cnt) : nextDayHistogramList xs
    LT -> error "forbidden negative age"

-- We need mapFromListWith because zeros becomes 6's, but also 7's become 6's
nextDayHistogram :: LanternFishHistogram -> LanternFishHistogram
nextDayHistogram h = Map.fromListWith (+) (nextDayHistogramList (Map.toList h))


fishCountAfterDaysOfHistogram :: LanternFishHistogram -> Int -> Int
fishCountAfterDaysOfHistogram h 0 = Map.foldl (+) 0 h
fishCountAfterDaysOfHistogram h b =
    fishCountAfterDaysOfHistogram (nextDayHistogram h) (b - 1)

fishCountAfterDays :: [Int] -> Int -> Int
fishCountAfterDays l d =
    let histogram = listToFishHistogram l
    in  fishCountAfterDaysOfHistogram histogram d
