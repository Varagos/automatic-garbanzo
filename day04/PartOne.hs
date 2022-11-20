module PartOne
    ( findBingoGameWinnerResult
    , findWinningBoardUnmarkedNumbersSum
    , checkIfMatrixWins
    ) where

import           Data.List                      ( concat
                                                , find
                                                , transpose
                                                )
import qualified Data.Set                      as Set


findBingoGameWinnerResult :: Foldable t => [Int] -> t [[Int]] -> Maybe Int
findBingoGameWinnerResult bingoNumbersInput matrixes =
    let initialSet = Set.empty
    in  findNumberThatWinsBingoGame bingoNumbersInput initialSet matrixes


findNumberThatWinsBingoGame
    :: Foldable t => [Int] -> Set.Set Int -> t [[Int]] -> Maybe Int
findNumberThatWinsBingoGame [] _ _ = Nothing
findNumberThatWinsBingoGame (freshNumber : futureInput) previousNumbers matrixes
    = case scanAllBoardsForAWinner matrixes updatedBingoNumbers freshNumber of
        Just winnerBoardResult -> Just winnerBoardResult
        Nothing ->
            findNumberThatWinsBingoGame futureInput updatedBingoNumbers matrixes
    where updatedBingoNumbers = Set.insert freshNumber previousNumbers



scanAllBoardsForAWinner matrixes bingoNumbers freshNumber =
    let maybeWinnerTable = find (checkIfMatrixWins bingoNumbers) matrixes
    in  case maybeWinnerTable of
            Just winnerTable -> Just $ finalResult winnerTable
            Nothing          -> Nothing
  where
    finalResult winnerTable =
        findWinningBoardUnmarkedNumbersSum winnerTable bingoNumbers
            * freshNumber

findWinningBoardUnmarkedNumbersSum :: [[Int]] -> Set.Set Int -> Int
findWinningBoardUnmarkedNumbersSum matrix bingoNumbers =
    let allMatrixNumbers = concat matrix
        unmarkedNumbers =
            Set.difference (Set.fromList allMatrixNumbers) bingoNumbers
    in  sum unmarkedNumbers


checkIfMatrixWins :: Set.Set Int -> [[Int]] -> Bool
checkIfMatrixWins bingoNumbers matrix =
    let rowsAndCols = getTableRowsAndColumns matrix
    in  any rowOrColWins rowsAndCols
  where
    rowOrColWins rowOrCol = Set.fromList rowOrCol `Set.isSubsetOf` bingoNumbers
    getTableRowsAndColumns table = table ++ transpose table


findWhyItWins bingoNumbers matrix =
    let rowsAndCols = getTableRowsAndColumns matrix
    in  find rowOrColWins rowsAndCols
  where
    rowOrColWins rowOrCol = Set.fromList rowOrCol `Set.isSubsetOf` bingoNumbers
    getTableRowsAndColumns table = table ++ transpose table


