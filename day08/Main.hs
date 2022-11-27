import           Lib                            ( countUniqueOutputDigits
                                                , decodeEntryOutput
                                                , processInput
                                                , wordsWhen
                                                )



main = do
    input <- readFile "input.txt"
    -- let  = map read $ wordsWhen (== '|') input :: [Int]
    -- Part One
    -- print inputData
    let inputData        = processInput input
    let fourDigitOutputs = map snd inputData
    -- print fourDigitOutputs
    print $ countUniqueOutputDigits fourDigitOutputs
    let fEntry : _ = inputData

    let outputsSum = sum $ map decodeEntryOutput inputData
    print outputsSum

