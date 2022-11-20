import           PartOne
import           PartTwo                        ( findUpdatedHazardousPoints )
import           ProcessInput                   ( fileContentToLines )
import           System.IO


main = do
    file    <- openFile "input.txt" ReadMode
    content <- hGetContents file
    let input = fileContentToLines content
    print $ findHazardousPoints input

    print $ findUpdatedHazardousPoints input

