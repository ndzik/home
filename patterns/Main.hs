module Main where

import Control.Monad (filterM, foldM)
import Data.Char (isLetter, toLower)
import Data.Foldable (forM_)
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, takeFileName, (</>))

-- --- File & Word Processing ---

-- Recursively finds all relevant source files, ignoring dependency directories.
getRelevantFiles :: FilePath -> IO [FilePath]
getRelevantFiles root = do
  paths <- listDirectory root
  let fullPaths = map (root </>) paths
  dirs <- filterM doesDirectoryExist fullPaths
  files <- filterM (fmap not . doesDirectoryExist) fullPaths
  let relevantFiles = filter hasRelevantExtension files
  let excludedDirs = ["pytorch-libs", "JUCE", "site-packages", "node_modules", ".git", ".stack-work", "dist-newstyle"]
  let isNotExcluded dir = takeFileName dir `notElem` excludedDirs
  let filteredDirs = filter isNotExcluded dirs
  filesFromSubdirs <- concat <$> mapM getRelevantFiles filteredDirs
  return (relevantFiles ++ filesFromSubdirs)
  where
    hasRelevantExtension path = takeExtension path `elem` [".hs", ".rs", ".py"]

-- Processes files sequentially to count word frequencies.
countWordsInFiles :: [FilePath] -> IO (Map String Int)
countWordsInFiles = foldM processFile Map.empty
  where
    processFile :: Map String Int -> FilePath -> IO (Map String Int)
    processFile !accCounts filePath = do
      putStrLn $ "Reading: " ++ filePath
      !content <- readFile filePath
      let fileWords = filter ((> 2) . length) . words . map toLower . filter isLetterOrSpace $ content
      let fileCounts = foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty fileWords
      return $ Map.unionWith (+) accCounts fileCounts
    isLetterOrSpace c = isLetter c || c == ' '

-- --- Sliding Window Comparison Logic (Refactored) ---

type SharedSegmentCounts = Map String Int

-- Main analysis function.
analyzeSharedSegments :: [String] -> SharedSegmentCounts
analyzeSharedSegments topWords =
  -- Iterate through every unique pair of words.
  foldl' (processPair topWords) Map.empty (zip [0 ..] topWords)
  where
    processPair :: [String] -> SharedSegmentCounts -> (Int, String) -> SharedSegmentCounts
    processPair allWords accCounts (idx, w1) =
      -- Compare w1 with every word that comes after it in the list.
      foldl' (\acc w2 -> findAndCountShared acc w1 w2) accCounts (drop (idx + 1) allWords)

-- For a given pair of words, find all shared substrings and update counts.
findAndCountShared :: SharedSegmentCounts -> String -> String -> SharedSegmentCounts
findAndCountShared counts w1 w2 =
  -- The range of offsets for sliding w2 relative to w1.
  let len1 = length w1
      len2 = length w2
      offsets = [-(len2 - 1) .. (len1 - 1)]
   in foldl' (\acc offset -> countSharedForOffset acc offset w1 w2) counts offsets

-- For a single alignment/offset, generate the bitmask and count the shared segments.
countSharedForOffset :: SharedSegmentCounts -> Int -> String -> String -> SharedSegmentCounts
countSharedForOffset counts offset w1 w2 =
  let (bitmask, aligned1, _) = generateAlignmentMask offset w1 w2
      sharedSegments = extractSharedSegments bitmask aligned1
   in foldl' (\acc segment -> Map.insertWith (+) segment 1 acc) counts sharedSegments

-- Generates a bitmask (1=same, 0=diff) for two words at a given offset.
generateAlignmentMask :: Int -> String -> String -> ([Int], String, String)
generateAlignmentMask offset w1 w2 =
  let (s1, s2) = align offset w1 w2
      -- Pad the shorter string to make them equal length for zipping.
      maxLen = max (length s1) (length s2)
      s1' = take maxLen (s1 ++ repeat ' ')
      s2' = take maxLen (s2 ++ repeat ' ')
      mask = zipWith (\c1 c2 -> if c1 == c2 && isLetter c1 then 1 else 0) s1' s2'
   in (mask, s1', s2')
  where
    -- Aligns two strings based on an offset.
    align o s1 s2
      | o >= 0 = (drop o s1, s2)
      | otherwise = (s1, drop (abs o) s2)

-- Given a bitmask and an aligned string, finds all groups of matching letters.
extractSharedSegments :: [Int] -> String -> [String]
extractSharedSegments bitmask str = go bitmask str
  where
    go [] _ = []
    go (1 : bms) (c : cs) =
      -- Found the start of a shared group.
      let (match, rest) = span ((== 1) . fst) (zip bms cs)
          segment = c : map snd match
       in -- Only count segments of length > 1
          if length segment > 1
            then segment : go (map fst rest) (map snd rest)
            else go (map fst rest) (map snd rest)
    go (_ : bms) (_ : cs) = go bms cs

-- --- Main ---

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dirPath] -> do
      putStrLn $ "Analyzing source files in directory: " ++ dirPath
      allFiles <- getRelevantFiles dirPath
      putStrLn $ "Found " ++ show (length allFiles) ++ " source files to analyze."

      wordCounts <- countWordsInFiles allFiles
      let sortedWordCounts = sortBy (\(_, c1) (_, c2) -> compare c2 c1) (Map.toList wordCounts)

      -- Take the top 1,000 words for this intensive analysis.
      -- (10,000 can be very slow, 1k is a good starting point).
      let topWords = map fst $ take 2_000 sortedWordCounts
      putStrLn $ "Analyzing patterns for top " ++ show (length topWords) ++ " words. This may take a while..."

      forM_ topWords $ \word ->
        putStrLn $ word

      let patternCounts = analyzeSharedSegments topWords
      let sortedPatterns = sortBy (\(_, c1) (_, c2) -> compare c2 c1) (Map.toList patternCounts)

      putStrLn "\n--- Top 50 Most Common Shared Word Segments ---"
      mapM_ print (take 50 sortedPatterns)

      putStrLn "\nAnalysis complete."
    _ -> putStrLn "Usage: cabal run home-patterns -- <path_to_directory>"
