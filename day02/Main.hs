import           System.IO

getLineInfo :: String -> (String, Int)
getLineInfo lineStr = (head segments, read (last segments))
    where segments = words lineStr
processInput :: [String] -> [(String, Int)]
processInput = map getLineInfo

main = do
    file    <- openFile "input.txt" ReadMode
    content <- hGetContents file
    let inputLines         = lines content
    let info               = processInput inputLines

    let depthAndHorizontal = foldl getLineDepthAndHorizontal (0, 0) info
    print $ fst depthAndHorizontal * snd depthAndHorizontal


getLineDepthAndHorizontal :: Num a => (a, a) -> ([Char], a) -> (a, a)
getLineDepthAndHorizontal (prevDepth, prevHor) (dir, val) = case dir of
    "forward" -> (prevDepth, prevHor + val)
    "down"    -> (prevDepth + val, prevHor)
    "up"      -> (prevDepth - val, prevHor)
    _         -> error "Unknown direction"


