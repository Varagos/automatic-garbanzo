import           Lib                            ( LanternFish(..)
                                                , findLanternFishAfterDays
                                                , wordsWhen
                                                )
import           PartTwo                        ( fishCountAfterDays )


main = do
    input <- readFile "input.txt"
    let initIntState   = map read $ wordsWhen (== ',') input
    let initFishSchool = map LanternFish initIntState
    let partOne        = length $ findLanternFishAfterDays initFishSchool 80
    print partOne

    let partTwo = fishCountAfterDays initIntState 256
    print partTwo
