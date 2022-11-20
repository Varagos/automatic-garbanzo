import           Data.Char                      ( digitToInt )
import           Data.List                      ( groupBy )
import           PartOne                        ( findBingoGameWinnerResult )
import           ProcessInput                   ( fileContentToNumbersAndMatrixes
                                                )
import           System.IO                      ( IOMode(ReadMode)
                                                , hGetContents
                                                , openFile
                                                )


main = do
    file    <- openFile "input.txt" ReadMode
    content <- hGetContents file
    let (numberSeries, matrixes) = fileContentToNumbersAndMatrixes content
    print $ findBingoGameWinnerResult numberSeries matrixes

