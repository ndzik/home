-- analysis/Main.hs
-- This program recursively scans a directory for source code files and analyzes
-- the frequency of adjacent letter groups (n-grams) by processing files sequentially.

module Main where

import Control.Monad (filterM, foldM, forM)
import Data.Char (isLetter, toLower)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.List (foldl', sort, sortBy, zip3, zip4)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as LT
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO

-- --- N-Gram Counting Logic (Unchanged) ---

type DigraphCounts = Map (Char, Char) Int

type TripletCounts = Map (Char, Char, Char) Int

type QuadrupletCounts = Map (Char, Char, Char, Char) Int

countQuadrupletsNotNormalized :: [String] -> QuadrupletCounts
countQuadrupletsNotNormalized words = foldl' processWord Map.empty words
  where
    processWord counts word = foldl' updateCounts counts (wordToQuadrupletsNotNormalized word)
    updateCounts counts quadruplet = Map.insertWith (+) quadruplet 1 counts

wordToQuadrupletsNotNormalized :: String -> [(Char, Char, Char, Char)]
wordToQuadrupletsNotNormalized word =
  let lettersOnly = filter isLetter $ map toLower word
   in zip4 lettersOnly (tail lettersOnly) (tail (tail lettersOnly)) (tail (tail (tail lettersOnly)))

countTripletsNotNormalized :: [String] -> TripletCounts
countTripletsNotNormalized words = foldl' processWord Map.empty words
  where
    processWord counts word = foldl' updateCounts counts (wordToTripletsNotNormalized word)
    updateCounts counts triplet = Map.insertWith (+) triplet 1 counts

wordToTripletsNotNormalized :: String -> [(Char, Char, Char)]
wordToTripletsNotNormalized word =
  let lettersOnly = filter isLetter $ map toLower word
   in zip3 lettersOnly (tail lettersOnly) (tail (tail lettersOnly))

countTriplets :: [String] -> TripletCounts
countTriplets words = foldl' processWord Map.empty words
  where
    processWord counts word = foldl' updateCounts counts (wordToTriplets word)
    updateCounts counts triplet = Map.insertWith (+) triplet 1 counts

wordToTriplets :: String -> [(Char, Char, Char)]
wordToTriplets word =
  let lettersOnly = filter isLetter $ map toLower word
   in map normalizeTriplet $ zip3 lettersOnly (tail lettersOnly) (tail (tail lettersOnly))
  where
    normalizeTriplet (a, b, c) = let sorted = sort [a, b, c] in (head sorted, sorted !! 1, last sorted)

countDigraphsNotNormalized :: [String] -> DigraphCounts
countDigraphsNotNormalized words = foldl' processWord Map.empty words
  where
    processWord counts word = foldl' updateCounts counts (wordToDigraphsNotNormalized word)
    updateCounts counts pair = Map.insertWith (+) pair 1 counts

wordToDigraphsNotNormalized :: String -> [(Char, Char)]
wordToDigraphsNotNormalized word =
  let lettersOnly = filter isLetter $ map toLower word
   in zip lettersOnly (tail lettersOnly)

countDigraphs :: [String] -> DigraphCounts
countDigraphs words = foldl' processWord Map.empty words
  where
    processWord counts word = foldl' updateCounts counts (wordToDigraphs word)
    updateCounts counts pair = Map.insertWith (+) pair 1 counts

wordToDigraphs :: String -> [(Char, Char)]
wordToDigraphs word =
  let lettersOnly = filter isLetter $ map toLower word
   in map normalizePair $ zip lettersOnly (tail lettersOnly)
  where
    normalizePair (a, b) = if a < b then (a, b) else (b, a)

-- --- File System Logic (Unchanged) ---

-- Recursively finds all files in a directory with the specified extensions,
-- ignoring specified directory names.
getRelevantFiles :: FilePath -> IO [FilePath]
getRelevantFiles root = do
  paths <- listDirectory root
  let fullPaths = map (root </>) paths
  dirs <- filterM doesDirectoryExist fullPaths
  files <- filterM (fmap not . doesDirectoryExist) fullPaths

  let relevantFiles = filter hasRelevantExtension files

  -- Filter out excluded directories before making the recursive call.
  let excludedDirs = ["site-packages", "node_modules", "target"]
  let isNotExcluded dir = takeFileName dir `notElem` excludedDirs
  let filteredDirs = filter isNotExcluded dirs

  filesFromSubdirs <- concat <$> mapM getRelevantFiles filteredDirs

  return (relevantFiles ++ filesFromSubdirs)
  where
    hasRelevantExtension :: FilePath -> Bool
    hasRelevantExtension path = takeExtension path `elem` [".hs", ".rs", ".py"]

-- --- Graphing Logic (Unchanged) ---

generateGraph :: DigraphCounts -> IO ()
generateGraph counts = do
  runGraphviz graph Png "digraph.png"
  putStrLn "\nGenerated graph visualization: digraph.png"
  where
    topN = 50
    sortedCounts = take topN $ sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList counts)
    graph = graphElemsToDot params nodes edges
    nodes = map (\c -> (c, [toLabel c])) ['a' .. 'z']
    maxCount = fromIntegral . maximum . map snd $ sortedCounts
    edges = map toEdge sortedCounts
    toEdge ((n1, n2), count) =
      ( n1,
        n2,
        [ PenWidth (fromIntegral count / maxCount),
          toLabel (show count)
        ]
      )
    params =
      nonClusteredParams
        { globalAttributes = [GraphAttrs [Layout Neato, Overlap ScaleOverlaps]],
          isDirected = False
        }

-- --- Main Application Logic (Refactored) ---

-- A type to hold all our count maps together.
type AllCounts = (DigraphCounts, TripletCounts, TripletCounts, QuadrupletCounts)

-- Processes a list of files sequentially, accumulating the n-gram counts.
processFiles :: [FilePath] -> IO AllCounts
processFiles files =
  -- Use a monadic fold to process files one by one, updating the accumulator.
  foldM processFile initialCounts files
  where
    -- The starting state for our fold: four empty maps.
    initialCounts :: AllCounts
    initialCounts = (Map.empty, Map.empty, Map.empty, Map.empty)

    -- The function to process a single file and update the total counts.
    processFile :: AllCounts -> FilePath -> IO AllCounts
    processFile (di, tri, triNorm, quadNorm) filePath = do
      putStrLn $ "Processing: " ++ filePath
      -- Using withFile ensures the file handle is closed automatically.
      withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        -- We use `length` to force evaluation of the contents while the handle is open.
        -- This is a common trick to ensure lazy IO completes.
        length contents `seq` return ()

        let allLines = lines contents

        -- Calculate counts for the current file.
        let di' = countDigraphsNotNormalized allLines
        let tri' = Map.empty -- countTriplets allLines
        let triNorm' = Map.empty -- countTripletsNotNormalized allLines
        let quadNorm' = Map.empty -- countQuadrupletsNotNormalized allLines

        -- Merge the current file's counts with the accumulated totals.
        -- Map.unionWith (+) is perfect for this.
        return
          ( Map.unionWith (+) di di',
            Map.unionWith (+) tri tri',
            Map.unionWith (+) triNorm triNorm',
            Map.unionWith (+) quadNorm quadNorm'
          )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dirPath] -> do
      putStrLn $ "Analyzing source files in directory: " ++ dirPath
      allFiles <- getRelevantFiles dirPath
      putStrLn $ "Found " ++ show (length allFiles) ++ " source files to analyze."

      (digraphCounts, tripletCounts, tripletCountsNotNormalized, quadrupletCountsNotNormalized) <- processFiles allFiles

      let sortedCounts = sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList digraphCounts)
      let sortedTripletCounts = sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList tripletCounts)
      let sortedTripletCountsNotNormalized = sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList tripletCountsNotNormalized)
      let sortedQuadrupletCountsNotNormalized = sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList quadrupletCountsNotNormalized)

      putStrLn "\n--- Top 50 Most Frequent Letter Pairs ---"
      mapM_ print (take 50 sortedCounts)

      putStrLn "\n--- Top 50 Most Frequent Letter Triplets ---"
      mapM_ print (take 50 sortedTripletCounts)

      putStrLn "\n--- Top 50 Most Frequent Letter Triplets (Not Normalized) ---"
      mapM_ print (take 50 sortedTripletCountsNotNormalized)

      putStrLn "\n--- Top 50 Most Frequent Letter Quadruplets (Not Normalized) ---"
      mapM_ print (take 50 sortedQuadrupletCountsNotNormalized)

      generateGraph digraphCounts

      putStrLn "\nAnalysis complete."
    _ -> putStrLn "Usage: cabal run home-analysis -- <path_to_directory>"
