{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Keymap
  ( KeyMaps (..),
    loadKeyMaps,
    baseKeycodeMap,
    symbolSequenceMap,
    outputKeyMap,
    keycodeToCharMap,
    CInt,
  )
where

import Data.Bits
import Data.Foldable (find)
import Data.List (zip4, zipWith4)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sort
import Data.Text qualified as T
import Data.Yaml (FromJSON (..), Value (..), decodeFileEither)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import Text.Layout.Table
import Text.Read (readMaybe)

-- The KeyMaps type now holds a single, unified map.
newtype KeyMaps = KeyMaps
  { keyMap :: Map [CInt] [CInt]
  }
  deriving (Show)

-- A helper type that matches the YAML structure.
newtype ParsedKeyMaps = ParsedKeyMaps
  { keymap :: Map T.Text [CInt]
  }
  deriving (Generic, Show)

instance FromJSON ParsedKeyMaps

instance FromJSON CInt where
  parseJSON (Number n) = return $ fromInteger $ round n
  parseJSON _ = fail "Expected a number for CInt"

convertParsedMaps :: ParsedKeyMaps -> Maybe KeyMaps
convertParsedMaps pkm = do
  let parsedTuples = Map.toList $ keymap pkm
  convertedTuples <- mapM parseTuple parsedTuples
  return $ KeyMaps (Map.fromList convertedTuples)
  where
    parseTuple :: (T.Text, [CInt]) -> Maybe ([CInt], [CInt])
    parseTuple (key, value) = do
      let keyStrings = T.splitOn "," key
      -- Sort the input keys to ensure consistent lookups.
      ks <- sort <$> mapM (readMaybe @CInt . T.unpack) keyStrings
      return (ks, value)

loadKeyMaps :: FilePath -> IO KeyMaps
loadKeyMaps path = do
  eResult <- decodeFileEither path
  case eResult of
    Left err -> do
      putStrLn $ "Error parsing YAML: " ++ show err
      exitFailure
    Right parsed ->
      case convertParsedMaps parsed of
        Nothing -> do
          putStrLn "Error converting parsed map keys to integers."
          exitFailure
        Just keyMaps -> do
          return keyMaps

keycodeToCharMap :: Map.Map CInt Char
keycodeToCharMap =
  Map.fromList
    [ (0, 'a'),
      (1, 's'),
      (2, 'd'),
      (3, 'f'),
      (5, 'g'),
      (4, 'h'),
      (38, 'j'),
      (40, 'k'),
      (37, 'l'),
      (12, 'q'),
      (13, 'w'),
      (14, 'e'),
      (15, 'r'),
      (17, 't'),
      (6, 'y'),
      (32, 'u'),
      (34, 'i'),
      (31, 'o'),
      (35, 'p'),
      (16, 'z'),
      (7, 'x'),
      (8, 'c'),
      (9, 'v'),
      (11, 'b'),
      (45, 'n'),
      (46, 'm'),
      (41, 'ö')
    ]

baseKeycodeMap :: Map.Map Char CInt
baseKeycodeMap =
  Map.fromList
    [ ('a', 0),
      ('s', 1),
      ('d', 2),
      ('f', 3),
      ('g', 5),
      ('h', 4),
      ('j', 38),
      ('k', 40),
      ('l', 37),
      ('q', 12),
      ('w', 13),
      ('e', 14),
      ('r', 15),
      ('t', 17),
      ('y', 6),
      ('u', 32),
      ('i', 34),
      ('o', 31),
      ('p', 35),
      ('z', 16),
      ('x', 7),
      ('c', 8),
      ('v', 9),
      ('b', 11),
      ('n', 45),
      ('m', 46),
      ('ö', 41)
    ]

symbolSequenceMap :: Map.Map Char [CInt]
symbolSequenceMap =
  Map.fromList
    [ ('<', [50]),
      ('>', [56, 50]),
      ('{', [61, 26]),
      ('}', [61, 25]),
      ('[', [61, 28]),
      (']', [61, 29]),
      ('(', [56, 28]),
      (')', [56, 25])
    ]

-- | Writes a table for the keymap to a file which visualizes the
-- keys to press for each output.
--
-- ◼️ -> not pressed
-- 🟩 -> pressed
--
-- Left and right hand are separated in their own columns.
outputKeyMap :: KeyMaps -> FilePath -> IO ()
outputKeyMap (KeyMaps km) path = do
  writeFile path $
    unlines
      [ "Keymap Table",
        "===========",
        "",
        "Left Hand Codes: 0, 1, 2, 3, 5",
        "Right Hand Codes: 4, 38, 40, 37, 41",
        "",
        "Legend:",
        "- = not pressed",
        "X = pressed",
        "",
        "Singles:",
        tableString $ mkKeymapTable 1,
        "",
        "Doubles:",
        tableString $ mkKeymapTable 2,
        "",
        "Triples:",
        tableString $ mkKeymapTable 3,
        "",
        "Quadruples:",
        tableString $ mkKeymapTable 4
      ]
  where
    leftHandCodes = [0, 1, 2, 3, 5]
    rightHandCodes = [4, 38, 40, 37, 41]
    -- All possible activations for 5 keys.
    relevantBitmaps n = filter (invalidOrImpossible n) $ generateBitmaps 5
    activityMap n =
      map
        ( unwords
            . map
              ( \case
                  0 -> "-"
                  1 -> "X"
              )
        )
        $ relevantBitmaps n

    output n codes = map (bitmapToOutput codes) $ relevantBitmaps n
    bitmapToOutput codes bs = outputKeys
      where
        outputKeys = case Map.lookup (sort activeKeys) km of
          Nothing -> "unmapped"
          Just rs -> map findKeycode rs
        findKeycode s = case find (\(_, c) -> c == s) $ Map.toList baseKeycodeMap of
          Nothing -> ' '
          Just (c, _) -> c
        activeKeys = filter (> (-1)) $ zipWith (\c b -> if b == 0 then -1 else c) codes bs

    cs =
      [ column (fixed 10) center def def,
        column (fixed 10) center def def,
        column (fixed 10) center def def,
        column (fixed 10) center def def
      ]
    headers = titlesH ["Left" :: String, "Output", "Right", "Output"]
    mkKeymapTable n =
      columnHeaderTableS cs unicodeS headers $ zipWith4 (\lact lo ract ro -> rowG [lact, lo, ract, ro]) (activityMap n) (output n leftHandCodes) (activityMap n) (output n rightHandCodes)

    -- \| Converts an integer to its n-bit binary representation as a list of 0s and 1s.
    --   The list is ordered from most significant bit to least significant.
    toBitmap :: Int -> Int -> [Int]
    toBitmap bitCount number =
      -- We map over the bit positions from left to right (e.g., for 5 bits: 4, 3, 2, 1, 0)
      map (\pos -> if testBit number pos then 1 else 0) [bitCount - 1, bitCount - 2 .. 0]

    -- \| Generates all possible n-bit bitmaps.
    generateBitmaps :: Int -> [[Int]]
    generateBitmaps bitCount =
      -- We map our toBitmap function over all numbers from 0 to (2^n - 1).
      map (toBitmap bitCount) [0 .. 2 ^ bitCount - 1]

    -- invalidOrImpossible bitmap = not $ sum bitmap == 0 || sum bitmap == 5
    invalidOrImpossible n bitmap = sum bitmap == n
