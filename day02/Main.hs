import           System.IO

getLineInfo :: String -> (String, Int)
getLineInfo lineStr = (head segments, read (last segments))
    where segments = words lineStr
processInput :: [String] -> [(String, Int)]
processInput = map getLineInfo

fstOf3 (a, _, _) = a
sndOf3 (_, a, _) = a

main = do
    file    <- openFile "input.txt" ReadMode
    content <- hGetContents file
    let inputLines         = lines content
    let info               = processInput inputLines

    let depthAndHorizontal = foldl reactToCommand (0, 0) info
    print $ uncurry (*) depthAndHorizontal

    let cords            = foldl handleCommand (0, 0, 0) info
    let newDepth         = fstOf3 cords
    let newHorizontalPos = sndOf3 cords
    print (newDepth * newHorizontalPos)


reactToCommand :: Num a => (a, a) -> ([Char], a) -> (a, a)
reactToCommand (prevDepth, prevHor) (direction, value) = case direction of
    "forward" -> (prevDepth, prevHor + value)
    "down"    -> (prevDepth + value, prevHor)
    "up"      -> (prevDepth - value, prevHor)
    _         -> error "Unknown direction"


handleCommand :: Num a => (a, a, a) -> ([Char], a) -> (a, a, a)
handleCommand (prevDepth, prevHor, prevAim) (direction, value) =
    case direction of
        "forward" -> (prevDepth + prevAim * value, prevHor + value, prevAim)
        "down"    -> (prevDepth, prevHor, prevAim + value)
        "up"      -> (prevDepth, prevHor, prevAim - value)
        _         -> error "Unknown direction"

