{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core qualified as C
import Control.Lens
import Data.Char qualified as Char
import Data.Foldable (find)
import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Graphics.Vty qualified as V
import Keymap

data AppState = AppState
  { _keyMaps :: KeyMaps,
    _wordList :: [String],
    _currentWord :: String,
    _currentIndex :: Int,
    _correctPresses :: Int,
    _totalPresses :: Int,
    _highlightedKeys :: Set.Set CInt
  }

makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI s =
  [ C.vBox
      [ drawFingers s,
        drawWord s,
        drawStats s
      ]
  ]

-- Draws the top row of highlighted fingers
drawFingers :: AppState -> Widget ()
drawFingers s =
  center $
    borderWithLabel (str " Fingers ") $
      C.vBox
        [ C.hBox $ map (drawKey s 0) [12, 13, 14, 15, 17] ++ [str "   "] ++ map (drawKey s 0) [16, 32, 34, 31, 35],
          C.hBox $ [str " "],
          C.hBox $ (map (drawKey s 1) [0, 1, 2, 3, 5] ++ [str "   "] ++ map (drawKey s 1) [4, 38, 40, 37, 41])
        ]
  where
    drawKey st offset key =
      let highlight =
            if Set.member key (st ^. highlightedKeys)
              then withAttr attrHighlight
              else id
          label = "[ ]"
       in case offset of
            0 -> hCenter $ C.hBox [highlight $ str label]
            os -> (hCenter $ C.hBox [highlight $ str label])

-- Draws the middle row with the word to type and the cursor
drawWord :: AppState -> Widget ()
drawWord s =
  center $
    C.vBox
      [ str (s ^. currentWord),
        str cursor
      ]
  where
    cursor = replicate (s ^. currentIndex) ' ' ++ "^"

-- Draws the bottom row with statistics
drawStats :: AppState -> Widget ()
drawStats s =
  center $
    C.vBox
      [ hBorder,
        C.padTop (C.Pad 1) $
          str $
            "Accuracy: " ++ show (round (accuracy * 100)) ++ "%"
      ]
  where
    accuracy =
      if s ^. totalPresses == 0
        then 0.0
        else fromIntegral (s ^. correctPresses) / fromIntegral (s ^. totalPresses)

app :: App AppState e ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr [(attrHighlight, V.black `on` V.white)]
    }

attrHighlight :: AttrName
attrHighlight = attrName "highlight"

handleEvent :: BrickEvent () e -> EventM () AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) = do
  s <- get
  let currentChar = s ^. currentWord . to (!! (s ^. currentIndex))
  case c of
    'q' -> halt -- Quit the application
    _ | c == currentChar -> do
      -- Correct key pressed, update state
      let s' =
            s
              & correctPresses +~ 1
              & totalPresses +~ 1
              & currentIndex +~ 1
       in put $ checkEndOfWord s'
    _ -> do
      -- Incorrect key pressed, just update total presses
      put $ s & totalPresses +~ 1
handleEvent _ = return ()

-- Logic to run when a word is completed
checkEndOfWord :: AppState -> AppState
checkEndOfWord s
  | s ^. currentIndex >= length (s ^. currentWord) =
      -- Reset with the next word in the list
      let (nextWord : remainingWords) = s ^. wordList
       in s
            & currentWord .~ nextWord
            & wordList .~ remainingWords
            & currentIndex .~ 0
            & highlightedKeys .~ getRequiredPresses nextWord (s ^. keyMaps)
  | otherwise =
      -- Update highlights for the next character
      let nextChar = (s ^. currentWord) !! (s ^. currentIndex)
       in s & highlightedKeys .~ getRequiredPresses [nextChar] (s ^. keyMaps)

-- --- Helper Functions ---

getRequiredPresses :: String -> KeyMaps -> Set.Set CInt
getRequiredPresses targetStr maps = fromMaybe Set.empty $ do
  targetChar <- safeHead targetStr
  -- Create the sequence of keycodes needed to produce the character.
  let desiredOutput = charToKeycodeSequence $ Char.toLower targetChar
  -- Find the input key combination that produces this output sequence.
  inputKeys <- fst <$> find (\(_, output) -> output == desiredOutput) (Map.toList $ keyMap maps)
  return $ Set.fromList inputKeys

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

charToKeycodeSequence :: Char -> [CInt]
charToKeycodeSequence c = fromMaybe [fromMaybe 0 (Map.lookup c baseKeycodeMap)] (Map.lookup c symbolSequenceMap)

-- --- Main ---

main :: IO ()
main = do
  putStrLn "Loading keymaps for TUI..."
  keymaps <- loadKeyMaps "keymap_config.yaml"
  outputKeyMap keymaps "keymap_table.txt"

  -- List of words with special chars for programming.
  let wordlist = cycle ["Haskell", "Brick{Keyboard}", "(Chording)", "Playground"]
  let firstWord = head wordlist

  let initialState =
        AppState
          { _keyMaps = keymaps,
            _wordList = tail wordlist,
            _currentWord = firstWord,
            _currentIndex = 0,
            _correctPresses = 0,
            _totalPresses = 0,
            _highlightedKeys = getRequiredPresses firstWord keymaps
          }

  _ <- defaultMain app initialState
  putStrLn "TUI exited."
