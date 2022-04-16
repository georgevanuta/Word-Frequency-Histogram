module Word where

type TWord = String
type TLetter = Char

--List of the useless characters that should
--never appear in a word
uselessLetters :: [TLetter]
uselessLetters = " $#@%^&*()[]{}+=_|\\\";:<>?/~`.,!?\n"

--Basic contains function for generic lists
contains :: (a -> Bool) -> [a] -> Bool
contains cond = foldr(\curr acc -> cond curr || acc) False

--Checks if a letter is useful
isLetterUseful :: TLetter -> Bool
isLetterUseful letter = not $ contains (== letter) uselessLetters




