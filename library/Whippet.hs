module Whippet (run, removeDups, exactMatches, inexactMatches) where

import Control.Monad (forM)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Regex (Regex, matchRegex, mkRegexWithOpts)

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

getRecursiveDirectoryContents :: FilePath -> IO [FilePath]
getRecursiveDirectoryContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveDirectoryContents path
      else return [path]
  return (concat paths)

run :: IO ()
run = do
  [query, directory] <- getArgs
  files              <- getRecursiveDirectoryContents directory
  putStr (unlines (removeDups (exactMatches query files) (inexactMatches query files)))
