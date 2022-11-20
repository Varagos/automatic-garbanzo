module PartTwo
    ( findBingoGameLastWinnerResult
    ) where
import qualified Data.Set                      as Set
import           GHC.Exts.Heap                  ( GenClosure(value) )
import           PartOne                        ( checkIfMatrixWins
                                                , findWinningBoardUnmarkedNumbersSum
                                                )


data Board = Board [[Int]]
    deriving Show

-- Each time a board wins, we filter it out from the next round. 
-- Otherwise it would win again is <= score

-- findBingoGameLastWinnerResult :: [Int] -> [[[Int]]] -> [Int]
findBingoGameLastWinnerResult bingoNumbersInput boards =
    last $ findWinningBingoBoardsResults bingoNumbersInput
                                         Set.empty
                                         (map Board boards)

-- findWinningBingoBoardsResults :: [Int] -> Set.Set Int -> [Board] -> [Int]
findWinningBingoBoardsResults :: [Int] -> Set.Set Int -> [Board] -> [Int]
findWinningBingoBoardsResults [] _ _  = []
findWinningBingoBoardsResults _  _ [] = []
findWinningBingoBoardsResults (freshNumber : futureInput) previousNumbers boards
    = let (currentRoundValues, indices) = unzip $ getRoundWinningValues
              boards
              (updatedBingoNumbers, freshNumber)
          remainingBoards = filterIndices indices boards

          nextRound       = findWinningBingoBoardsResults futureInput
                                                          updatedBingoNumbers
                                                          remainingBoards
      in  currentRoundValues ++ nextRound
    --   in  remainingBoards
    where updatedBingoNumbers = Set.insert freshNumber previousNumbers



        -- filter (`elem` indices) boards

filterIndices :: [Int] -> [Board] -> [Board]
filterIndices []      boards = boards
filterIndices indices boards = snd (foldr keepOrThrow (lastIndex, []) boards)
  where
    keepOrThrow :: Board -> (Int, [Board]) -> (Int, [Board])
    keepOrThrow x (index, acc) = if index `elem` indices
        then (nextIndex, acc)
        else (nextIndex, x : acc)
        where nextIndex = index - 1
    lastIndex = length boards - 1

getRoundWinningValues :: [Board] -> (Set.Set Int, Int) -> [(Int, Int)]
getRoundWinningValues boards (bingoNumbers, freshNumber) =
    let winnerTables = markWinningTableIndices boards bingoNumbers 0
    in  valueOfWinnerBoards winnerTables
  where
    valueOfWinnerBoards = map
        (\(Board winnerBoard, index) ->
            ( findWinningBoardUnmarkedNumbersSum winnerBoard bingoNumbers
                * freshNumber
            , index
            )
        )

markWinningTableIndices :: [Board] -> Set.Set Int -> Int -> [(Board, Int)]
markWinningTableIndices [] bingoNumbers index = []
markWinningTableIndices (fstBoard@(Board firstBoard) : tailBoards) bingoNumbers index
    = if checkIfMatrixWins bingoNumbers firstBoard
        then (fstBoard, index)
            : markWinningTableIndices tailBoards bingoNumbers (index + 1)
        else markWinningTableIndices tailBoards bingoNumbers (index + 1)


