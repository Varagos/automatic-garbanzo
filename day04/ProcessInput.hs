module ProcessInput
    ( fileContentToNumbersAndMatrixes
    ) where

import           Data.List                      ( groupBy )



wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'



fileContentToNumbersAndMatrixes content =
    let fileLines        = lines content
        numbersString    = head fileLines
        gameBody         = tail fileLines
        numberStrings    = wordsWhen (== ',') numbersString
        numberSeries     = map read numberStrings :: [Int]
        matrixesOfString = map
            (map words)
            (filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") gameBody)

        matrixes = map (map (map read)) matrixesOfString :: [[[Int]]]
    in  (numberSeries, matrixes)
