import           Lib                            ( findImprovedLeastPossibleFuelPosition
                                                , findLeastPossibleFuelPosition
                                                , wordsWhen
                                                )



main = do
    input <- readFile "input.txt"
    let horizontalPositions = map read $ wordsWhen (== ',') input :: [Int]
    -- Part One
    print $ findLeastPossibleFuelPosition horizontalPositions

    print $ findImprovedLeastPossibleFuelPosition horizontalPositions

