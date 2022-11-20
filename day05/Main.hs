import           PartOne
import           ProcessInput                   ( fileContentToLines )
import           System.IO


main = do
    file    <- openFile "input.txt" ReadMode
    content <- hGetContents file
    let input = fileContentToLines content
    print $ findHazardousPoints input

