import           Data.Char                      ( digitToInt )
import           System.IO

main = do
    inputFile <- openFile "input.txt" ReadMode
    fileData  <- hGetContents inputFile
    let binaryMetrics = lines fileData
    let gammaRate     = calculateGammaRate binaryMetrics
    let epsilonRate   = calculateEpsilonRate binaryMetrics
    let powerConsumption =
            binaryStrToDec gammaRate * binaryStrToDec epsilonRate
    print powerConsumption


-- binaryStrToDecStr :: Num a => [Char] -> Int
-- binaryStrToDecStr [    x     ] = [x]
binaryStrToDec :: [Char] -> Int
binaryStrToDec [x] = digitToInt x
binaryStrToDec all@(x : xs) =
    (digitToInt x * 2) ^ (length all - 1) + binaryStrToDec xs
binaryStrToDec _ = error "weird input i guess"


calculateGammaRate :: Eq a => [[a]] -> [a]
calculateGammaRate input = calculateRate input 0 mostCommonElem

calculateEpsilonRate :: Eq a => [[a]] -> [a]
calculateEpsilonRate input = calculateRate input 0 leastCommonElem

calculateRate binaryInputs index pickElemStrategy
    | index == wordMaxIndex
    = [currentPositionElemWinner]
    | index < wordMaxIndex
    = currentPositionElemWinner
        : calculateRate binaryInputs (index + 1) pickElemStrategy
    | otherwise
    = error "Index overflow"
  where
    wordMaxIndex              = length (head binaryInputs) - 1
    currentPositionElemWinner = pickElemStrategy (map (!! index) binaryInputs)


timesOfElem :: (Num a, Eq t) => t -> [t] -> a
timesOfElem _ [] = 0
timesOfElem a (x : xs) | a == x    = 1 + timesOfElem a xs
                       | otherwise = 0 + timesOfElem a xs
filterFromList :: Eq a => [a] -> a -> [a]
filterFromList xs x = [ elem | elem <- xs, elem /= x ]

mostCommonElem :: Eq t => [t] -> t
mostCommonElem []  = error "Empty list does not have a most common element"
mostCommonElem [x] = x
mostCommonElem all@(x : xs)
    | listHasOnlyXs = x
    | timesOfElem x all >= timesOfElem (mostCommonElem listWithoutX) xs = x
    | otherwise     = mostCommonElem listWithoutX
  where
    listWithoutX  = filterFromList xs x
    listHasOnlyXs = null listWithoutX

-- 11100
leastCommonElem :: Eq t => [t] -> t
leastCommonElem []  = error "Empty list does not have a least common element"
leastCommonElem [x] = x
leastCommonElem all@(x : xs)
    | listHasOnlyXs = x
    | x `timesOfElem` all <= timesOfElem (leastCommonElem listWithoutX) xs = x
    | otherwise     = leastCommonElem listWithoutX
  where
    listWithoutX  = filterFromList xs x
    listHasOnlyXs = null listWithoutX


-- 3 <
