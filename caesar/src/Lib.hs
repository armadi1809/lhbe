module Lib
  ( lowerAlphabet,
    upperAlphabet,
    digits,
    rot135,
  )
where

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

indexOf :: Char -> Alphabet -> Int
indexOf _ [] = -1
indexOf c (x : xs) = if x == c then 0 else 1 + indexOf c xs

isLower :: Char -> Bool
isLower c = c `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper c = c `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit c = c `elem` digits

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
  alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

digitRot :: Int -> Char -> Char
digitRot n ch = alphabetRot digits n ch

rot135Char :: Char -> Char
rot135Char ch
  | isUpper ch = upperRot 13 ch
  | isLower ch = lowerRot 13 ch
  | isDigit ch = digitRot 5 ch
  | otherwise = ch

rot135 :: String -> String
rot135 message = map (rot135Char) message