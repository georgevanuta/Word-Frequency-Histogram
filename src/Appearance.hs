module Appearance where

import Data.List (sortBy)

import Word(TWord, contains)
type TAppearance = (TWord, Integer)

increaseAppearences :: TWord -> [TAppearance] -> [TAppearance]
increaseAppearences word listOfWords
    | contains (\x -> (fst x) == word) listOfWords = foldr(\curr acc -> if fst curr == word then (fst curr, snd curr + 1):acc else curr:acc) [] listOfWords
    | otherwise = (word,1):listOfWords

appearancesList :: [TWord] -> [TAppearance]
appearancesList = foldr increaseAppearences []

appearancesComp :: TAppearance -> TAppearance -> Ordering
appearancesComp ta1 ta2
        | snd ta1 > snd ta2 = LT
        | snd ta1 < snd ta2 = GT
        | otherwise = EQ

sortAppearances :: [TAppearance] -> [TAppearance]
sortAppearances = sortBy appearancesComp
