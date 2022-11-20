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
    putStrLn $ "Power consumptions is: " ++ show powerConsumption

    -- Second Part
    let oxygenGeneratorRating =
            findRatingValue binaryMetrics 0 oxygenGeneratorCriteria
    let co2ScrubberRating = findRatingValue binaryMetrics 0 co2ScrubberCriteria
    let submarineLifeSupportRating = oxygenGeneratorRating * co2ScrubberRating
    putStrLn
        $  "Life support rating of submarine is: "
        ++ show submarineLifeSupportRating


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

mostCommonElem :: Eq t => [t] -> t
mostCommonElem []  = error "Empty list does not have a most common element"
mostCommonElem [x] = x
mostCommonElem all@(x : xs)
    | listHasOnlyXs = x
    | timesOfElem x all >= timesOfElem (mostCommonElem listWithoutX) xs = x
    | otherwise     = mostCommonElem listWithoutX
  where
    listWithoutX  = filter (/= x) xs
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
    listWithoutX  = filter (/= x) xs
    listHasOnlyXs = null listWithoutX


-- Second part

findRatingValue :: [[Char]] -> Int -> ([[Char]] -> Int -> [[Char]]) -> Int
findRatingValue [] _ _ = error "No number provided"
findRatingValue [x] _ _ = binaryStrToDec x
findRatingValue (x : _) bitPos _ | bitPos > length x = error "Index overflow"
findRatingValue xs bitPos bitCriteria =
    let keptNumbers = bitCriteria xs bitPos
    in  findRatingValue keptNumbers (bitPos + 1) bitCriteria

-- findRatingValue []  _ _ = error "String is empty"
-- findRatingValue [x] _ _ = [x]
-- findRatingValue (x : xs) bitPos bitCriteria
--     | bitCriteria x bitPos = x : findRatingValue xs (bitPos + 1) bitCriteria
--     | otherwise            = findRatingValue xs (bitPos + 1) bitCriteria


-- Will return only numbers with mostCommonValue in asked bitPosition
oxygenGeneratorCriteria :: [[Char]] -> Int -> [[Char]]
oxygenGeneratorCriteria numbers bitPos =
    let bitsOfPosition = foldl (\acc x -> (x !! bitPos) : acc) [] numbers
        timesOfOne     = '1' `timesOfElem` bitsOfPosition
        timesOfZero    = '0' `timesOfElem` bitsOfPosition
        mostCommonBit | timesOfOne >= timesOfZero = '1'
                      | otherwise                 = '0'
    in  filter (\x -> x !! bitPos == mostCommonBit) numbers

-- Will return only numbers with leastCommonValue in asked bitPosition
co2ScrubberCriteria :: [[Char]] -> Int -> [[Char]]
co2ScrubberCriteria numbers bitPos =
    let bitsOfPosition = foldl (\acc x -> (x !! bitPos) : acc) [] numbers
        timesOfOne     = '1' `timesOfElem` bitsOfPosition
        timesOfZero    = '0' `timesOfElem` bitsOfPosition
        leastCommonBit | timesOfZero <= timesOfOne = '0'
                       | otherwise                 = '1'
    in  filter (\x -> x !! bitPos == leastCommonBit) numbers
