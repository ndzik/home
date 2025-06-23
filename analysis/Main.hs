module Main where

import Data.Char (isLetter, toLower)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.List (foldl', sort, sortBy, zip3, zip4)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as LT
import System.IO

type DigraphCounts = Map (Char, Char) Int

type TripletCounts = Map (Char, Char, Char) Int

type QuadrupletCounts = Map (Char, Char, Char, Char) Int

countQuadrupletsNotNormalized :: [String] -> QuadrupletCounts
countQuadrupletsNotNormalized words = foldl' processWord Map.empty words
  where
    processWord :: Map (Char, Char, Char, Char) Int -> String -> Map (Char, Char, Char, Char) Int
    processWord counts word = foldl' updateCounts counts (wordToQuadrupletsNotNormalized word)

    updateCounts :: Map (Char, Char, Char, Char) Int -> (Char, Char, Char, Char) -> Map (Char, Char, Char, Char) Int
    updateCounts counts quadruplet = Map.insertWith (+) quadruplet 1 counts

wordToQuadrupletsNotNormalized :: String -> [(Char, Char, Char, Char)]
wordToQuadrupletsNotNormalized word =
  let lettersOnly = filter isLetter $ map toLower word
   in zip4 lettersOnly (tail lettersOnly) (tail (tail lettersOnly)) (tail (tail (tail lettersOnly)))

countTripletsNotNormalized :: [String] -> TripletCounts
countTripletsNotNormalized words = foldl' processWord Map.empty words
  where
    processWord :: Map (Char, Char, Char) Int -> String -> Map (Char, Char, Char) Int
    processWord counts word = foldl' updateCounts counts (wordToTripletsNotNormalized word)

    updateCounts :: Map (Char, Char, Char) Int -> (Char, Char, Char) -> Map (Char, Char, Char) Int
    updateCounts counts triplet = Map.insertWith (+) triplet 1 counts

wordToTripletsNotNormalized :: String -> [(Char, Char, Char)]
wordToTripletsNotNormalized word =
  let lettersOnly = filter isLetter $ map toLower word
   in zip3 lettersOnly (tail lettersOnly) (tail (tail lettersOnly))

countTriplets :: [String] -> TripletCounts
countTriplets words = foldl' processWord Map.empty words
  where
    processWord :: Map (Char, Char, Char) Int -> String -> Map (Char, Char, Char) Int
    processWord counts word = foldl' updateCounts counts (wordToTriplets word)

    updateCounts :: Map (Char, Char, Char) Int -> (Char, Char, Char) -> Map (Char, Char, Char) Int
    updateCounts counts triplet = Map.insertWith (+) triplet 1 counts

-- Converts a single word into a list of normalized, sorted letter triplets.
wordToTriplets :: String -> [(Char, Char, Char)]
wordToTriplets word =
  let lettersOnly = filter isLetter $ map toLower word
   in map normalizeTriplet $ zip3 lettersOnly (tail lettersOnly) (tail (tail lettersOnly))
  where
    normalizeTriplet :: (Char, Char, Char) -> (Char, Char, Char)
    normalizeTriplet (a, b, c) = let sorted = sort [a, b, c] in (head sorted, sorted !! 1, last sorted)

-- Processes a list of words and returns the frequency map of letter pairs.
countDigraphs :: [String] -> DigraphCounts
countDigraphs words = foldl' processWord Map.empty words
  where
    processWord :: DigraphCounts -> String -> DigraphCounts
    processWord counts word = foldl' updateCounts counts (wordToDigraphs word)

    updateCounts :: DigraphCounts -> (Char, Char) -> DigraphCounts
    updateCounts counts pair = Map.insertWith (+) pair 1 counts

-- Converts a single word into a list of normalized, sorted letter pairs.
wordToDigraphs :: String -> [(Char, Char)]
wordToDigraphs word =
  let lettersOnly = filter isLetter $ map toLower word
   in map normalizePair $ zip lettersOnly (tail lettersOnly)
  where
    normalizePair :: (Char, Char) -> (Char, Char)
    normalizePair (a, b) = if a < b then (a, b) else (b, a)

generateGraph :: DigraphCounts -> IO ()
generateGraph counts = do
  -- Render the graph to an SVG file.
  runGraphviz graph Png "digraph.png"
  putStrLn "\nGenerated graph visualization: digraph.svg"
  where
    topN = 50 -- Visualize the top N strongest connections
    sortedCounts = take topN $ sortBy (\(_, countA) (_, countB) -> compare countB countA) (Map.toList counts)
    -- Define the graph structure using the graphviz library's EDSL.
    graph = graphElemsToDot params nodes edges
    -- Define all letters as nodes.
    nodes = map (\c -> (c, [toLabel c])) ['a' .. 'z']
    maxCount = fromIntegral . maximum $ map snd sortedCounts

    -- Convert our frequency counts into weighted edges.
    -- The edge thickness (penwidth) will represent the frequency.
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

main :: IO ()
main = do
  putStrLn "Analyzing digraph frequency in 10,000 common English words..."

  handle <- openFile "resources/wordlist-10_000.txt" ReadMode
  contents <- hGetContents handle
  let words = lines contents
  let digraphCounts = countDigraphs words
  let tripletCounts = countTriplets words
  let tripletCountsNotNormalized = countTripletsNotNormalized words
  let quadrupletCountsNotNormalized = countQuadrupletsNotNormalized words

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

  -- Generate the graph visualization.
  generateGraph digraphCounts

  hClose handle
  putStrLn "\nAnalysis complete."
