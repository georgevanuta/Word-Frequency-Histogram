module Sentence where

import Word
import Data.Char (toLower)

type TSentence = String
--Shifts the sentence one word to the left
skipWord :: TSentence -> TSentence
skipWord (letter:rest)
    | isLetterUseful letter = skipWord rest
    | otherwise = letter:rest
skipWord _ = []

--Removes all leading white spaces
getToNextWord :: TSentence -> TSentence
getToNextWord (letter:rest)
    | isLetterUseful letter = letter:rest
    | otherwise = getToNextWord rest
getToNextWord _ = []

--Returns the first word of a sentence
getNextWord :: TSentence -> TWord
getNextWord (letter:rest)
    | isLetterUseful letter = letter:(getNextWord rest)
    | otherwise = []
getNextWord _ = []

--Splits a sentence into words
splitSentence :: TSentence -> [TWord]
splitSentence [] = []
splitSentence sentence = (map toLower $ getNextWord sentence):(splitSentence $ getToNextWord $ skipWord sentence)
