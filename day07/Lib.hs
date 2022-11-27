module Lib
    ( wordsWhen
    , findLeastPossibleFuelPosition
    ) where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'

-- findLeastPossibleFuelPosition :: [Int] -> Int
findLeastPossibleFuelPosition initPos =
    let minPos               = foldr1 min initPos
        maxPos               = foldr1 max initPos
        allPossiblePositions = [minPos .. maxPos]
        fuelForAllPos        = [ calcFuelForPos x | x <- allPossiblePositions ]
    in  foldr1 min fuelForAllPos
    where calcFuelForPos pos = sum [ abs (x - pos) | x <- initPos ]
