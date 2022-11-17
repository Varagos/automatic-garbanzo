import           System.IO

convertToIntArr :: [String] -> [Int]
convertToIntArr = map read

main = do
    file  <- openFile "input.txt" ReadMode
    input <- hGetContents file
    let inputs  = lines input
    let numbers = convertToIntArr inputs
    print $ countTimesDepthIncreases numbers

    print $ threeMeasurementIncreases numbers


countTimesDepthIncreases :: [Int] -> Int

countTimesDepthIncreases []  = 0

countTimesDepthIncreases [x] = 0

countTimesDepthIncreases (x1 : x2 : xs)
    | x2 > x1   = 1 + countTimesDepthIncreases (x2 : xs)
    | otherwise = 0 + countTimesDepthIncreases (x2 : xs)


threeMeasurementIncreases :: [Int] -> Int

threeMeasurementIncreases []           = 0

threeMeasurementIncreases [x]          = 0
threeMeasurementIncreases [x1, x2]     = 0
threeMeasurementIncreases [x1, x2, x3] = 0
threeMeasurementIncreases (x1 : x2 : x3 : x4 : xs)
    | x2 + x3 + x4 > x1 + x2 + x3 = 1
    + threeMeasurementIncreases (x2 : x3 : x4 : xs)
    | otherwise = 0 + threeMeasurementIncreases (x2 : x3 : x4 : xs)
