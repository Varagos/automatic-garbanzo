{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib
    ( wordsWhen
    , LanternFish(..)
    , LanternFishSchool(..)
    , findLanternFishAfterDays
    ) where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'

data LanternFish = LanternFish Int
    deriving (Show, Eq, Ord)

type LanternFishSchool = [LanternFish]


nextDayFish :: LanternFish -> (LanternFish, Maybe LanternFish)
nextDayFish (LanternFish a) = case a `compare` 0 of
    GT -> (LanternFish (a - 1), Nothing)
    EQ -> (LanternFish 6, Just newLanternFish)
    LT -> error "forbidden negative age"
    where newLanternFish = LanternFish 8

findLanternFishAfterDays :: LanternFishSchool -> Int -> LanternFishSchool
findLanternFishAfterDays a 0 = a
findLanternFishAfterDays a b =
    let nextDayFishSchool = map nextDayFish a
        parentFish        = map fst nextDayFishSchool
        maybeChildren     = map snd nextDayFishSchool
        newChildren       = foldr
            (\m acc -> case m of
                Just x  -> x : acc
                Nothing -> acc
            )
            ([])
            maybeChildren
    in  findLanternFishAfterDays (parentFish ++ newChildren) (b - 1)
