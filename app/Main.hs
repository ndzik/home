module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad (forever, when)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign.C.Types
import Foreign.Ptr
import Keymap

-- --- FFI Section ---

type KeyCallback = CInt -> CInt -> IO CInt

foreign import ccall "wrapper"
  mkKeyCallback :: KeyCallback -> IO (FunPtr KeyCallback)

foreign import ccall safe "key_listener.h start_key_listener"
  c_start_key_listener :: FunPtr KeyCallback -> IO CInt

foreign import ccall unsafe "key_listener.h send_key_event"
  c_send_key_event :: CInt -> CBool -> IO ()

-- --- Chording Logic ---

type ChordState = TVar (Set CInt)

debounceTime :: Int
debounceTime = 30000 -- 30 ms

runDebouncer :: KeyMaps -> ChordState -> IO ()
runDebouncer keyMaps state = do
  threadDelay debounceTime
  keys <- atomically $ do
    keys <- readTVar state
    writeTVar state Set.empty
    return keys

  let keyList = Set.toList keys

  -- Look up the pressed key combination (single or chord) in the unified map.
  case Map.lookup keyList (keyMap keyMaps) of
    Just outputKeys -> do
      putStrLn $ "[Haskell] Input: " ++ show keyList ++ " -> Output: " ++ show outputKeys
      sendKeySequence outputKeys
    Nothing -> do
      -- If the combination is not mapped, send the original keys as a fallback.
      when (not . null $ keyList) $
        putStrLn $
          "[Haskell] Unmapped input (sending as-is): " ++ show keyList
      sendKeySequence keyList

sendKeySequence :: [CInt] -> IO ()
sendKeySequence [] = return ()
sendKeySequence outputKeys = do
  -- Separate modifiers from the main key.
  let isModifier k = k `elem` [56, 60, 59, 55, 58, 61] -- LShift, RShift, LCtrl, LCmd, LAlt, RAlt
  let (modifiers, mainKeys) = span isModifier outputKeys

  -- Press all modifiers down
  mapM_ (\k -> c_send_key_event k 1) modifiers
  threadDelay 10000

  -- Press and release all main keys
  mapM_ (\k -> c_send_key_event k 1 >> threadDelay 5000 >> c_send_key_event k 0) mainKeys

  -- Release all modifiers
  threadDelay 10000
  mapM_ (\k -> c_send_key_event k 0) modifiers

myHaskellCallback :: KeyMaps -> ChordState -> KeyCallback
myHaskellCallback keyMaps state eventType keyCode
  | eventType == 10 = do
      atomically $ modifyTVar' state (Set.insert keyCode)
      forkIO $ runDebouncer keyMaps state
      return 1
  | otherwise = return 1

main :: IO ()
main = do
  putStrLn "[Haskell] Initializing chording engine..."

  keyMaps <- loadKeyMaps "keymap_config.yaml"
  putStrLn "[Haskell] Keymaps loaded successfully from YAML."

  chordState <- newTVarIO Set.empty
  callbackPtr <- mkKeyCallback (myHaskellCallback keyMaps chordState)
  result <- c_start_key_listener callbackPtr

  if result /= 0
    then putStrLn "[Haskell] Error: The C library failed to start."
    else do
      putStrLn "[Haskell] C library started. Chording engine is active."
      putStrLn "[Haskell] Press Ctrl+C to exit."
      forever $ threadDelay (10 ^ 6)

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = readTVar var >>= writeTVar var . f
