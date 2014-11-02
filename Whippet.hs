import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import Text.Regex

matches :: Regex -> [[Char]] -> [[Char]]
matches regex strings = [s | s <- strings, not $ isNothing $ match regex s]

exactMatches :: [Char] -> [[Char]] -> [[Char]]
exactMatches query strings = matches (stringToRegex query) strings

inexactMatches :: [Char] -> [[Char]] -> [[Char]]
inexactMatches query strings = matches (stringToFuzzyRegex query) strings

match :: Regex -> [Char] -> Maybe[String]
match query string = matchRegex query string

notMultiline = True
caseSensitive = False

stringToRegex :: [Char] -> Regex
stringToRegex string = mkRegexWithOpts string notMultiline caseSensitive

stringToFuzzyRegex :: [Char] -> Regex
stringToFuzzyRegex string = mkRegexWithOpts (intercalate ".*" (splitOn "" string)) notMultiline caseSensitive

removeDups :: [[Char]] -> [[Char]] -> [[Char]]
removeDups exacts inExacts = exacts ++ [s | s <- inExacts, not $ s `elem` exacts]

main = do
  [query, directory] <- getArgs
  files              <- getDirectoryContents directory
  putStr (unlines (removeDups (exactMatches query files) (inexactMatches query files)))
