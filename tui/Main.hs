{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick as B hiding (on)
import Brick qualified as B
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core qualified as C
import Control.Lens
import Data.Char qualified as Char
import Data.Foldable (find, forM_)
import Data.Function (on)
import Data.List (find, inits, intercalate, intersperse, isPrefixOf, sortBy, tails)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Graphics.Vty qualified as V
import Keymap
import System.Exit (exitSuccess)
import System.Random

data Hand = LeftHand | RightHand | NoHand deriving (Show, Eq)

data AppState = AppState
  { _keyMaps :: KeyMaps,
    _wordList :: [String],
    _currentWord :: String,
    _currentIndex :: Int,
    _correctPresses :: Int,
    _totalPresses :: Int,
    _highlightedKeys :: Set.Set CInt,
    _outputToInputMap :: Map.Map [CInt] [[CInt]], -- For efficient lookups
    _recommendation :: Maybe String, -- To store the current tip
    _rng :: StdGen,
    _allPossibleKeys :: [[CInt]], -- All possible keys for the current expected input
    _hands :: [Hand] -- To track which hand is currently active
  }

makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI s =
  [ C.vBox
      [ drawFingers s,
        drawWord s,
        drawStats s,
        drawRecommendation s
      ]
  ]

isLeftHand :: CInt -> Bool
isLeftHand key = key `elem` leftHandKeys

isRightHand :: CInt -> Bool
isRightHand key = key `elem` rightHandKeys

leftHandKeys, rightHandKeys :: [CInt]
leftHandKeys = [0, 1, 2, 3, 5]
rightHandKeys = [4, 38, 40, 37, 41]

-- Draws the top row of highlighted fingers
drawFingers :: AppState -> Widget ()
drawFingers s =
  center $
    borderWithLabel (str " Fingers ") $
      C.vBox
        [ C.hBox $ map (drawKey s) [12, 13, 14, 15, 17] ++ [fill ' '] ++ map (drawKey s) [16, 32, 34, 31, 35],
          C.hBox $ map (drawKey s) [0, 1, 2, 3, 5] ++ [fill ' '] ++ map (drawKey s) [4, 38, 40, 37, 41]
        ]
  where
    drawKey st key =
      let highlight =
            if Set.member key (st ^. highlightedKeys)
              then withAttr attrHighlight
              else id
          label = "[ ]"
       in hCenter $ C.hBox [highlight $ str label]

attrHighlight :: AttrName
attrHighlight = attrName "highlight"

-- Draws the middle row with the word to type and the cursor
drawWord :: AppState -> Widget ()
drawWord s = center $ C.vBox [str (s ^. currentWord), str cursor]
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
            "Accuracy: " ++ show (round (accuracy * 100)) ++ "%",
        C.padTop (C.Pad 1) $
          str $
            "Last hand: " ++ show ((s ^. hands) !! 1),
        C.padTop (C.Pad 1) $ str "Canidates:",
        C.vBox $
          map
            ( \keys ->
                str . show $ map showKey keys
            )
            (s ^. allPossibleKeys)
      ]
  where
    showKey key =
      fromMaybe '?' (Map.lookup key keycodeToCharMap)
    accuracy =
      if s ^. totalPresses == 0
        then 0.0
        else fromIntegral (s ^. correctPresses) / fromIntegral (s ^. totalPresses)

-- Draws the recommendation widget
drawRecommendation :: AppState -> Widget ()
drawRecommendation s =
  center $
    C.padTop (C.Pad 1) $
      withAttr attrRecommendation $
        str $
          fromMaybe "" (s ^. recommendation)

attrRecommendation :: AttrName
attrRecommendation = attrName "recommendation"

app :: App AppState e ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (attrHighlight, V.black `B.on` V.white),
              (attrRecommendation, fg V.cyan)
            ]
    }

handleEvent :: BrickEvent () e -> EventM () AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) = do
  s <- get
  let currentChar = s ^. currentWord . to (!! (s ^. currentIndex))
  case c of
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

checkEndOfWord :: AppState -> AppState
checkEndOfWord s
  | s ^. currentIndex >= length (s ^. currentWord) = randomNextWord s
  | otherwise =
      findRecommendation s -- Find recommendation for the new position

randomNextWord :: AppState -> AppState
randomNextWord s =
  let s' =
        s
          & currentWord .~ (s ^. wordList) !! randomIdx
          & currentIndex .~ 0
          & rng .~ rng' -- Update the random generator
          & hands .~ [NoHand, NoHand] -- Reset hands
          & allPossibleKeys .~ [] -- Reset possible keys
   in findRecommendation s' -- Find recommendation for the new word
  where
    -- Use the rng contained in AppState to select a random word.
    (randomIdx, rng') = randomR (0, length (s ^. wordList) - 1) (s ^. rng)

type TypingOption = (Set.Set CInt, String, Hand)

findRecommendation :: AppState -> AppState
findRecommendation s =
  let remainingWord = Char.toLower <$> drop (s ^. currentIndex) (s ^. currentWord)
      -- 1. Find ALL possible ways to type prefixes of the remaining word.
      allOptions = findPossibleTypingOptions remainingWord (s ^. outputToInputMap)
      -- 2. Score them based on hand alternation and length.
      bestOption = selectBestOption ((s ^. hands) !! 0) allOptions
   in case bestOption of
        -- 3. Update the UI with the best recommendation found.
        Just (inputKeys, matchedPrefix, newHand) ->
          let tip =
                if length matchedPrefix > 1
                  then Just $ "Tip: Type '" ++ matchedPrefix ++ "' with " ++ formatKeys (Set.toList inputKeys)
                  else Nothing
           in s
                & recommendation .~ tip
                & highlightedKeys .~ inputKeys
                -- Prepend the new hand to the history
                & hands %~ (newHand :) -- Add the new hand to the history
                & allPossibleKeys .~ [Set.toList inputKeys] -- Update possible keys
        Nothing ->
          s
            & recommendation .~ Just "Error: No mapping found!"
            & highlightedKeys .~ Set.empty
            -- Reset hands to NoHand
            & hands .~ [NoHand, NoHand]

-- Finds all possible ways (single keys or chords) to type prefixes of a string.
findPossibleTypingOptions :: String -> Map.Map [CInt] [[CInt]] -> [TypingOption]
findPossibleTypingOptions remainingWord invertedMap =
  let possiblePrefixes = filter (not . null) . inits $ remainingWord
   in concatMap (prefixToOption invertedMap) possiblePrefixes
  where
    prefixToOption :: Map.Map [CInt] [[CInt]] -> String -> [TypingOption]
    prefixToOption invMap prefix =
      let desiredOutput = stringToKeycodeSequence prefix
       in case Map.lookup desiredOutput invMap of
            Just inputChordList ->
              map (\input -> (Set.fromList input, prefix, getHandForKey (Set.fromList input))) inputChordList
            Nothing -> []

getHandForKey :: Set.Set CInt -> Hand
getHandForKey keys
  | not $ Set.null $ Set.intersection keys (Set.fromList leftHandKeys) = LeftHand
  | not $ Set.null $ Set.intersection keys (Set.fromList rightHandKeys) = RightHand
  | otherwise = NoHand

-- Scores and selects the best option from a list.
selectBestOption :: Hand -> [TypingOption] -> Maybe TypingOption
selectBestOption _ [] = Nothing
selectBestOption lastHand options = Just . snd . head $ sorted
  where
    scoredOptions = map (\opt@(_, prefix, hand) -> (scoreOption lastHand prefix hand, opt)) options
    sorted = sortBy compareOptions scoredOptions
    compareOptions (scoreA, (_, prefixA, _)) (scoreB, (_, prefixB, _)) =
      compare scoreB scoreA <> compare (length prefixB) (length prefixA)

    -- Scoring logic: strongly prefer hand alternation.
    scoreOption :: Hand -> String -> Hand -> Int
    scoreOption NoHand prefix hand = 100 + length prefix -- Initial state, no preference
    scoreOption LeftHand prefix RightHand = 200 + length prefix -- Perfect alternation
    scoreOption RightHand prefix LeftHand = 200 + length prefix -- Perfect alternation
    scoreOption _ prefix _ = 0 + length prefix -- Same hand, lowest score

getRequiredPressesForSingleChar :: Char -> KeyMaps -> Set.Set CInt
getRequiredPressesForSingleChar char maps = fromMaybe Set.empty $ do
  let desiredOutput = charToKeycodeSequence (Char.toLower char)
  inputKeys <- fst <$> find (\(_, output) -> output == desiredOutput) (Map.toList $ keyMap maps)
  return $ Set.fromList inputKeys

formatKeys :: [CInt] -> String
formatKeys keys =
  let keyNames = map (\k -> fromMaybe '?' (Map.lookup k keycodeToCharMap)) keys
   in intersperse '+' keyNames

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

stringToKeycodeSequence :: String -> [CInt]
stringToKeycodeSequence = concatMap charToKeycodeSequence

charToKeycodeSequence :: Char -> [CInt]
charToKeycodeSequence c = fromMaybe [fromMaybe 0 (Map.lookup c baseKeycodeMap)] (Map.lookup c symbolSequenceMap)

-- --- Main ---

main :: IO ()
main = do
  putStrLn "Loading keymaps for TUI..."
  keymaps <- loadKeyMaps "keymap_config.yaml"
  outputKeyMap keymaps "keymap_table.txt"

  exitSuccess

  -- List of words with special chars for programming.
  let firstWord = head someWords

  -- Create the inverted map for efficient lookups (output -> input)
  let invertedMap = Map.fromListWith (++) $ map (\(k, v) -> (v, [k])) $ Map.toList $ keyMap keymaps

  randomNumber <- randomIO :: IO Int

  -- Initial state is found by running the recommendation on the first word.
  let s0 =
        AppState
          { _keyMaps = keymaps,
            _wordList = someWords,
            _currentWord = firstWord,
            _currentIndex = 0,
            _correctPresses = 0,
            _totalPresses = 0,
            _highlightedKeys = Set.empty, -- Will be set by findRecommendation
            _outputToInputMap = invertedMap,
            _recommendation = Nothing,
            _rng = mkStdGen randomNumber, -- Random generator for selecting words
            _allPossibleKeys = [], -- Will be set by findRecommendation
            _hands = [NoHand, NoHand] -- Start with no hand active
          }
  let initialState = findRecommendation s0

  _ <- defaultMain app initialState
  putStrLn "TUI exited."

someWords =
  [ "Haskell",
    "Brick{Keyboard}",
    "(Chording)",
    "Playground",
    "class",
    "when",
    "string",
    "used",
    "long",
    "may",
    "license",
    "fail",
    "none",
    "list",
    "returns",
    "must",
    "which",
    "count",
    "should",
    "number",
    "buffer",
    "result",
    "index",
    "code",
    "input",
    "only",
    "uintmax",
    "default",
    "float",
    "check",
    "using",
    "ret",
    "let",
    "key",
    "self",
    "have",
    "typename",
    "but",
    "ctx",
    "hpdefault",
    "endif",
    "source",
    "auto",
    "import",
    "get",
    "see",
    "include",
    "other",
    "new",
    "copyright",
    "software",
    "len",
    "testerror",
    "one",
    "mut",
    "test",
    "has",
    "output",
    "create",
    "while",
    "juce",
    "entry",
    "copy",
    "dataset",
    "str",
    "err",
    "its",
    "advance",
    "glenum",
    "values",
    "param",
    "pointer",
    "call",
    "first",
    "flags",
    "buf",
    "assert",
    "override",
    "method",
    "hidt",
    "public",
    "note",
    "localname",
    "gluint",
    "args",
    "message",
    "memory",
    "length",
    "array",
    "property",
    "namespace",
    "intt",
    "device",
    "into",
    "node",
    "block",
    "version",
    "group",
    "without",
    "pass",
    "then",
    "current",
    "line",
    "same",
    "noexcept",
    "allownoncamelcasetypes",
    "ifndef",
    "read",
    "start",
    "crate",
    "callback",
    "window",
    "path",
    "order",
    "need",
    "under",
    "target",
    "purpose",
    "was",
    "template",
    "each",
    "bytes",
    "after",
    "virtual",
    "glint",
    "also",
    "given",
    "status",
    "whether",
    "table",
    "mode",
    "called",
    "idx",
    "arg",
    "offset",
    "herrt",
    "double",
    "where",
    "there",
    "provided",
    "context",
    "functions",
    "inline",
    "more",
    "time",
    "module",
    "terms",
    "format",
    "user",
    "these",
    "space",
    "bit",
    "write",
    "some",
    "ptr",
    "since",
    "match",
    "open",
    "make",
    "close",
    "failed",
    "point",
    "than",
    "including",
    "xff",
    "handle",
    "before",
    "part",
    "info",
    "does",
    "here",
    "brief",
    "add",
    "dest",
    "private",
    "information",
    "res",
    "glsizei",
    "been",
    "ifdef",
    "types",
    "hsizet",
    "attribute",
    "ctensor",
    "next",
    "text",
    "val",
    "dim",
    "because",
    "xdefine",
    "typedef",
    "dont",
    "range",
    "nullptr",
    "specified",
    "parameter",
    "enum",
    "reusable",
    "hiinvalidhid",
    "endstate",
    "header",
    "just",
    "api",
    "internal",
    "either",
    "paramin",
    "operator",
    "dukuintt",
    "tag",
    "field",
    "found",
    "integertoi",
    "integertoitestallownonsnakecasefn",
    "link",
    "library",
    "returned",
    "failure",
    "ref",
    "following",
    "model",
    "stream",
    "expected",
    "options",
    "event",
    "objects",
    "datatype",
    "passed",
    "free",
    "retvalue",
    "otherwise",
    "image",
    "available",
    "dtype",
    "parameters",
    "example",
    "width",
    "required",
    "access",
    "last",
    "cache",
    "printf",
    "except",
    "component",
    "params",
    "zero",
    "defined",
    "torchtensor",
    "stack",
    "shift",
    "success",
    "obj",
    "cant",
    "kind",
    "process",
    "argument",
    "failstackerror",
    "already",
    "filename",
    "tmp",
    "bits",
    "implied",
    "done",
    "reserved",
    "lexstate",
    "being",
    "level",
    "invalid",
    "now",
    "switch",
    "support",
    "empty",
    "valid",
    "arguments",
    "utfprocindicconjunctbreaknone",
    "two",
    "reference",
    "utfprocboundclassother",
    "about",
    "base",
    "chunk",
    "element",
    "created",
    "left",
    "above",
    "hdf",
    "elif",
    "location",
    "khronosapientry",
    "files",
    "stdstring",
    "position",
    "always",
    "address",
    "src",
    "they",
    "thread",
    "glfloat",
    "supported",
    "continue",
    "shape",
    "vector",
    "torchrandn",
    "rights",
    "devicedevice",
    "such",
    "instead",
    "flag",
    "pragma",
    "change",
    "heap",
    "framework",
    "contains",
    "warranties",
    "request",
    "xfxf",
    "delete",
    "instance",
    "elements",
    "integer",
    "structure",
    "local",
    "would",
    "your",
    "inputs",
    "distribution",
    "byte",
    "cannot",
    "row",
    "different",
    "variable",
    "dims",
    "dataspace",
    "notice",
    "metadata",
    "specific",
    "stride",
    "item",
    "raise",
    "smallstate",
    "selection",
    "try",
    "loop",
    "optional",
    "between",
    "verify",
    "func",
    "update",
    "both",
    "operation",
    "kwargs",
    "vec",
    "height",
    "hfailed",
    "root",
    "section",
    "right",
    "main",
    "token",
    "filter",
    "utfprocbidiclassl",
    "tests",
    "mask",
    "currently",
    "tokenizer",
    "original",
    "single",
    "xbe",
    "like",
    "doesnt",
    "fields",
    "licensed",
    "bignum",
    "skip",
    "hash",
    "work",
    "through",
    "creation",
    "run",
    "tree",
    "impl",
    "client",
    "apientryp",
    "them",
    "store",
    "layout",
    "constexpr",
    "xbd",
    "identifier",
    "mauint",
    "details",
    "creates",
    "avoid",
    "distributed",
    "payment",
    "possible",
    "raw",
    "argnone",
    "find",
    "rank",
    "remove",
    "xxx",
    "explicit",
    "conditions",
    "needed",
    "sets",
    "option",
    "warranty",
    "project",
    "even",
    "top",
    "cursor",
    "written",
    "uint",
    "content",
    "properties",
    "added",
    "calls",
    "page",
    "config",
    "obtain",
    "fapl",
    "implementation",
    "unable",
    "authors",
    "apientry",
    "parent",
    "within",
    "final",
    "ctensors",
    "over",
    "character",
    "allocate",
    "maximum",
    "names",
    "pintuintuintuterm",
    "want",
    "pintuintuintuintuterm",
    "were",
    "entries",
    "padding",
    "could",
    "full",
    "express",
    "multiple",
    "below",
    "msg",
    "allocation",
    "max",
    "scale",
    "record",
    "externallexstate",
    "limited",
    "sure",
    "application",
    "iterator",
    "policy",
    "fprintfstderr",
    "color",
    "errors",
    "what",
    "checkret",
    "channel",
    "child",
    "step",
    "compliance",
    "cpu",
    "allocated",
    "once",
    "negative",
    "calling",
    "thr",
    "reset",
    "samepoutput",
    "apache",
    "program",
    "succeed",
    "still",
    "tensors",
    "customer",
    "language",
    "global",
    "query",
    "shared",
    "extern",
    "second",
    "subject",
    "weight",
    "initself",
    "load",
    "dimension",
    "dst",
    "packstatic",
    "named",
    "fill",
    "lambda",
    "uses",
    "storage",
    "system",
    "special",
    "connection",
    "graph",
    "forwardself",
    "actual",
    "num",
    "todo",
    "register",
    "addr",
    "way"
  ]
