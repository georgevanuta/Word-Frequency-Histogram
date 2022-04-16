module Histogram where

import Appearance
import Word (TWord)
import Sentence

type Bar = [Char]
type Histogram = String 

--The full bar of 80 '#'
fullBar :: Bar 
fullBar = take 80 $ repeat '#'

--Max length of word from a list of appearances
maxLengthOfWord :: [TAppearance] -> Integer 
maxLengthOfWord = foldr(\curr acc -> if toInteger (length (fst curr)) > acc then toInteger (length (fst curr)) else acc) (-1)

--Adds a number of whitespaces to an appearance
addWhiteSpaces :: Integer -> TAppearance  -> TAppearance  
addWhiteSpaces n app = ((fst app) ++ (replicate (fromInteger (n)) ' '), snd app) 

fitWord :: Integer -> TAppearance -> TAppearance 
fitWord n app = addWhiteSpaces x app
    where x = n - toInteger (length (fst app))

--Makes all words in an appearance list the same
--length by adding whitespaces
makeAllWordsTheSameLength :: [TAppearance] -> [TAppearance]
makeAllWordsTheSameLength apps = map (fitWord $ maxLengthOfWord apps) apps 

--Scales a number from a range to another range
scale :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 
scale x frmMin frmMax toMin toMax = (x - frmMin) * (toMax - toMin) `div` ((frmMax - frmMin) + toMin)

--Scales a bar so it fits on the screen
--max 80 '#'
scaleBar :: Integer -> Integer -> Bar 
scaleBar x oldMax = take (fromInteger $ scale x 0 oldMax 0 80) fullBar

--Builds a histogram from a list of appearances
histogramAux :: [TAppearance] -> Histogram 
histogramAux apps = foldr(\curr acc ->  if (length (scaleBar (snd curr) oldMax) > 0) then (fst curr) ++ " " ++ (scaleBar (snd curr) oldMax) ++ "\n" ++ acc else acc) [] apps
    where oldMax = maxLengthOfWord apps

--Builds a histogram from a sentence
histogram :: TSentence -> String 
histogram = histogramAux . makeAllWordsTheSameLength . sortAppearances . appearancesList . splitSentence
