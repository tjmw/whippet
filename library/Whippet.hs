module Whippet (run, removeDups, exactMatches, inexactMatches) where

import Control.Applicative
import Control.Monad (forM)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import Data.Strings (strNull)
import Options
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Regex (Regex, matchRegex, mkRegexWithOpts)

data MainOptions = MainOptions {
  optPath    :: String,
  optQuery   :: String,
  optExclude :: String
}

instance Options MainOptions where
	defineOptions = pure MainOptions
		<*> simpleOption "path" "."
		    "Path to search from"
		<*> simpleOption "query" ""
		    "Search query"
		<*> simpleOption "exclude" ""
		    "Exclude paths"

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

parseExcludes :: [Char] -> [[Char]]
parseExcludes excludes = splitOn "," excludes

getRecursiveDirectoryContents :: FilePath -> [[Char]] -> IO [FilePath]
getRecursiveDirectoryContents topdir excludes = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` excludes) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveDirectoryContents path excludes
      else return [path]
  return (concat paths)

run :: IO ()
run = runCommand $ \opts args -> do
  let query = optQuery opts
  let path = optPath opts
  let excludes = [".", ".."] ++ (parseExcludes (optExclude opts))
  files <- getRecursiveDirectoryContents path excludes
  if (strNull query)
    then putStr (unlines files)
    else putStr (unlines (removeDups (exactMatches query files) (inexactMatches query files)))
