module ProcessInput
    ( fileContentToLines
    ) where
import           Lib                            ( Line
                                                , lineFromXY
                                                )

import           Data.List                      ( groupBy )
import           Data.Text                      ( pack
                                                , splitOn
                                                , unpack
                                                )



wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'

fileContentToLines :: [Char] -> [Line]
fileContentToLines content =
    let fileLines       = lines content

        breakArrowParts = concatMap (splitOn (pack "->") . pack) fileLines
        numberStrings   = map (filter (/= ' ') . unpack) breakArrowParts
    in  inputNumsToLines numberStrings

inputNumsToLines :: [String] -> [Line]
inputNumsToLines []  = []
inputNumsToLines [x] = error "cant create point with 1 cord"
inputNumsToLines (point1 : point2 : restPoints) =
    let [x1, y1] = map read $ wordsWhen (== ',') point1
        [x2, y2] = map read $ wordsWhen (== ',') point2
    in  lineFromXY (x1, y1) (x2, y2) : inputNumsToLines restPoints
