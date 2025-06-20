{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Keymap
  ( KeyMaps (..),
    loadKeyMaps,
    CInt,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Yaml (FromJSON (..), Value (..), decodeFileEither)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
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

-- Convert the parsed YAML data into our clean KeyMaps.
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
      ks <- mapM (readMaybe @CInt . T.unpack) keyStrings
      return (ks, value)

-- IO action to load and parse the keymap from the YAML file.
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
