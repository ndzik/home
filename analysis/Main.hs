module Main where

import Data.Char (toLower)
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO

-- The core data structure: a map from a sorted character pair to its count.
type DigraphCounts = Map (Char, Char) Int

-- Processes a list of words and returns the frequency map of letter pairs.
countDigraphs :: [String] -> DigraphCounts
countDigraphs words = foldl' processWord Map.empty words
  where
    -- For each word, update the total counts with the pairs from this word.
    processWord :: DigraphCounts -> String -> DigraphCounts
    processWord counts word = foldl' updateCounts counts (wordToDigraphs word)

    -- For each pair, increment its count in the map.
    updateCounts :: DigraphCounts -> (Char, Char) -> DigraphCounts
    updateCounts counts pair = Map.insertWith (+) pair 1 counts

-- Converts a single word into a list of normalized, sorted letter pairs.
-- Example: "Hello" -> [('e','h'), ('l','l'), ('l','o')]
wordToDigraphs :: String -> [(Char, Char)]
wordToDigraphs word =
  -- Using `zip` with `tail` is an elegant way to get all adjacent pairs.
  -- e.g., zip "word" (tail "word") -> [('w','o'), ('o','r'), ('r','d')]
  map normalizePair $ zip (map toLower word) (map toLower $ tail word)
  where
    -- Normalizes a pair to be sorted alphabetically.
    -- This ensures that ('t', 'h') and ('h', 't') are counted as the same edge.
    normalizePair :: (Char, Char) -> (Char, Char)
    normalizePair (a, b) = if a < b then (a, b) else (b, a)

main :: IO ()
main = do
  putStrLn "Analyzing digraph frequency in 10,000 common English words..."

  -- Read the word list file.
  handle <- openFile "resources/wordlist-10_000.txt" ReadMode
  contents <- hGetContents handle
  let words = lines contents

  -- Perform the analysis.
  let digraphCounts = countDigraphs words

  -- Sort the results by frequency in descending order for readability.
  let sortedCounts = sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList digraphCounts)

  putStrLn "\n--- Top 50 Most Frequent Letter Pairs ---"
  mapM_ print (take 50 sortedCounts)

  hClose handle
  putStrLn "\nAnalysis complete."
