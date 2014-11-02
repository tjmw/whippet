import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import Text.Regex

exactMatches :: [Char] -> [[Char]] -> [[Char]]
exactMatches query strings = [s | s <- strings, not $ isNothing $ match query s]

match :: [Char] -> [Char] -> Maybe[String]
match query string = matchRegex (stringToRegex query) string

notMultiline = True
caseSensitive = False

stringToRegex :: [Char] -> Regex
stringToRegex string = mkRegexWithOpts string notMultiline caseSensitive

main = do
  [query, directory] <- getArgs
  files              <- getDirectoryContents directory
  putStr (unlines (exactMatches query files))
