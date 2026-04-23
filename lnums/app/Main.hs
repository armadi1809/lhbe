module Main (main) where

import Data.Char (isPrint, isSeparator)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs, getProgName)

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

data PadMode = PadLeft | PadRight

data LineNumberOptions = ReverseNumbering | SkipEmptyLines | LeftAlign deriving (Eq)

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

lnOptionFromString :: String -> Maybe LineNumberOptions
lnOptionFromString "--reverse" = Just ReverseNumbering
lnOptionFromString "--skip-empty" = Just SkipEmptyLines
lnOptionFromString "--left-align" = Just LeftAlign
lnOptionFromString _ = Nothing

parseArguments :: [String] -> (Maybe FilePath, [LineNumberOptions])
parseArguments [] = (Nothing, [])
parseArguments (filename : options) = (Just filename, mapMaybe lnOptionFromString options)

isEmpty :: String -> Bool
isEmpty str = null str || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty str = not (isEmpty str)

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber text =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs) =
        let mNumbering = if shouldNumber x then Just counter else Nothing
            newCounter = if shouldIncr x then counter + 1 else counter
         in (mNumbering, x) : go newCounter xs
   in go 1 text

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

-- numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
-- numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty

readLines :: FilePath -> IO [String]
readLines filePath = do
  content <- readFile filePath
  return (lines content)

pad :: PadMode -> Int -> String -> String
pad mode n str =
  let diff = n - length str
      padding = replicate diff ' '
   in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding

-- padLeft :: Int -> String -> String
-- padLeft = pad PadLeft

-- padRight :: Int -> String -> String
-- padRight = pad PadRight

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode lineNums =
  let (numbers, text) = unzip lineNums
      numberStrings = map (maybe "" show) numbers
      maxLength = maximum (map length numberStrings)
      paddedNumbers = map (pad mode maxLength) numberStrings
   in zipWith (\n l -> n ++ " " ++ l) paddedNumbers text

main :: IO ()
main = do
  cliArgs <- getArgs
  let (mFilePath, options) = parseArguments cliArgs
      numberFunction =
        if SkipEmptyLines `elem` options
          then numberNonEmptyLines
          else numberAllLines
      padMode =
        if LeftAlign `elem` options
          then PadRight
          else PadLeft
      go filePath = do
        fileLines <- readLines filePath
        let numbered = numberFunction fileLines
            prettyNumbered = prettyNumberedLines padMode numbered
            revNumbered = numberFunction (reverse fileLines)
            revPretty = reverse (prettyNumberedLines padMode revNumbered)
        mapM_ putStrLn (if ReverseNumbering `elem` options then revPretty else prettyNumbered)

  maybe
    (printHelpText "Missing filename")
    go
    mFilePath
