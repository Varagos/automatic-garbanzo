module Lib
    ( SignalPattern
    , wordsWhen
    , processInput
    , countUniqueOutputDigits
    , decodeEntryOutput
    ) where
import qualified Data.Set                      as Set

import qualified Data.Map                      as Map
import           Data.Maybe

type SignalPattern = String
type FourDigitOutputValue = [String]
type Entry = ([SignalPattern], FourDigitOutputValue)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'

processInput :: String -> [Entry]
processInput input = map (processEntryParts . wordsWhen (== '|')) $ lines input
  where
    processEntryParts :: [String] -> Entry
    processEntryParts [a, b] = (words a, words b)
    processEntryParts _      = error "Line has more than 2 parts"


-- 2 segments / 1
-- 3 segments / 7
-- 4 segments / 4
-- 5 segments / 2, 3, 5
-- 6 segments / 0, 6, 9
-- 7 segments / 8

-- a, b, c, d, e, f, g
-- 1, 4, 7, 8




countUniqueOutputDigits :: [FourDigitOutputValue] -> Int
countUniqueOutputDigits []       = 0
countUniqueOutputDigits (x : xs) = length (filter isUniqueDigit x)
    + countUniqueOutputDigits xs
  where
    isUniqueDigit :: String -> Bool
    isUniqueDigit a = length a `elem` [2, 3, 4, 7]


getSignalOfLength n = head . filter ((== n) . length)

get1 :: [SignalPattern] -> SignalPattern
get1 = getSignalOfLength 2

get7 :: [SignalPattern] -> SignalPattern
get7 = getSignalOfLength 3

get4 :: [SignalPattern] -> SignalPattern
get4 = getSignalOfLength 4

get8 :: [SignalPattern] -> SignalPattern
get8 = getSignalOfLength 7


get235 :: [SignalPattern] -> [SignalPattern]
get235 = filter ((== 5) . length)


get069 :: [SignalPattern] -> [SignalPattern]
get069 = filter ((== 6) . length)



-- Whatever is on 7 and not on 1 , matches -> a
--[c, f] from 1, whatever is not on oneOf 0,6,9 (6), matches -> c
-- the one left -> matches f
-- [bcdf] from 4, deduct [c,f] -> [b,d]
-- [b,d] whatever is on all of 2,3,5 matches -> d
-- the one left -> matches b

-- We have a,b,  c, d, f
-- We need to find e and g
-- e is on 0, 2, 6, 8
-- g is on 0, 2, 3, 5, 6, 8, 9
-- The one on both 2, 3, 5, matches -> g
-- THe one left is e

decodeEntryOutput :: Entry -> Int
decodeEntryOutput (signals, output) = read $ decodeOutput output

  where
    (a, b, c, d, e, f, g) = decodeSegments signals
    decodeOutput :: FourDigitOutputValue -> String
    decodeOutput [] = []
    decodeOutput (x : xs) | isZero x  = '0' : decodeOutput xs
                          | isOne x   = '1' : decodeOutput xs
                          | isTwo x   = '2' : decodeOutput xs
                          | isThree x = '3' : decodeOutput xs
                          | isFour x  = '4' : decodeOutput xs
                          | isFive x  = '5' : decodeOutput xs
                          | isSix x   = '6' : decodeOutput xs
                          | isSeven x = '7' : decodeOutput xs
                          | isEight x = '8' : decodeOutput xs
                          | isNine x  = '9' : decodeOutput xs
                          | otherwise = error "Unknown output"
    isZero :: String -> Bool
    isZero x =
        length x == 6 && Set.fromList x == Set.fromList [a, b, c, e, f, g]
    isOne x = length x == 2 && Set.fromList x == Set.fromList [c, f]
    isTwo x = length x == 5 && Set.fromList x == Set.fromList [a, c, d, e, g]
    isThree x = length x == 5 && Set.fromList x == Set.fromList [a, c, d, f, g]
    isFour x = length x == 4 && Set.fromList x == Set.fromList [b, c, d, f]
    isFive x = length x == 5 && Set.fromList x == Set.fromList [a, b, d, f, g]
    isSix x =
        length x == 6 && Set.fromList x == Set.fromList [a, b, d, e, f, g]
    isSeven x = length x == 3 && Set.fromList x == Set.fromList [a, c, f]
    isEight x =
        length x == 7 && Set.fromList x == Set.fromList [a, b, c, d, e, f, g]
    isNine x =
        length x == 6 && Set.fromList x == Set.fromList [a, b, c, d, f, g]


-- 2 segments / 1
-- 3 segments / 7
-- 4 segments / 4
-- 5 segments / 2, 3, 5
-- 6 segments / 0, 6, 9
-- 7 segments / 8



-- decodeEntryOutput :: Entry -> Int
decodeSegments :: [SignalPattern] -> (Char, Char, Char, Char, Char, Char, Char)
decodeSegments signals =
    let a      = getA
        c      = getC
        f      = getF c
        d      = getD
        b      = getB d
        eAndGi = [ x | x <- ['a' .. 'g'], x `notElem` [a, b, c, d, f] ]
        g      = getG eAndGi
        e      = head [ x | x <- eAndGi, x /= g ]
    in  (a, b, c, d, e, f, g)

  where
    (one, twoThreeFive, four, seven, eight, zeroSixNine) =
        ( get1 signals
        , get235 signals
        , get4 signals
        , get7 signals
        , get8 signals
        , get069 signals
        )
    getA = head [ x | x <- seven, x `notElem` one ]
    getC = head [ x | x <- one, timesOf x (concat zeroSixNine) == 2 ]
    getF c = head [ x | x <- one, x /= c ]
    getD =
        head
            [ x
            | x <- four
            , x `notElem` one
            , timesOf x (concat twoThreeFive) == 3
            ]
    getB d = head [ x | x <- four, x `notElem` one, x /= d ]
    getG eAndGi =
        head [ x | x <- eAndGi, timesOf x (concat twoThreeFive) == 3 ]

    timesOf a c = length [ x | x <- c, x == a ]


