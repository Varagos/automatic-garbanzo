module Lib
    ( wordsWhen
    , findLeastPossibleFuelPosition
    , findImprovedLeastPossibleFuelPosition
    ) where

import qualified Data.Map                      as Map
import           Data.Maybe

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'

findLeastPossibleFuelPosition :: [Int] -> Int
findLeastPossibleFuelPosition initPos =
    let minPos               = foldr1 min initPos
        maxPos               = foldr1 max initPos
        allPossiblePositions = [minPos .. maxPos]
        fuelForAllPos        = [ calcFuelForPos x | x <- allPossiblePositions ]
    in  foldr1 min fuelForAllPos
    where calcFuelForPos pos = sum [ abs (x - pos) | x <- initPos ]


-- findImprovedLeastPossibleFuelPosition :: [Int] -> Int
findImprovedLeastPossibleFuelPosition :: [Int] -> Int
findImprovedLeastPossibleFuelPosition initPos =
    let
        minPos               = foldr1 min initPos
        maxPos               = foldr1 max initPos
        allPossiblePositions = [minPos .. maxPos]

        fuelBook             = stepsCost 0 (abs (maxPos - minPos)) Map.empty
        fuelForAllPos =
            [ calcTotalFuelForPos x fuelBook | x <- allPossiblePositions ]
    in
        foldr1 min fuelForAllPos
  where
    calcTotalFuelForPos pos fuelBook =
        sum [ fromMaybe 0 (Map.lookup (abs (x - pos)) fuelBook) | x <- initPos ]



stepsCost :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
stepsCost 0 max map = stepsCost 1 max map
stepsCost totalSteps max fuelBook | totalSteps > max = fuelBook
stepsCost totalSteps max fuelBook =
    let previousCost     = fromMaybe 0 $ Map.lookup (totalSteps - 1) fuelBook
        currentTotalCost = previousCost + totalSteps
        updatedFuelBook  = Map.insert totalSteps currentTotalCost fuelBook
    in  stepsCost (totalSteps + 1) max updatedFuelBook

    -- costMap = Map.empty
