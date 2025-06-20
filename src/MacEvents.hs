module MacEvents where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.IO

-- Opaque pointers to CoreFoundation/CoreGraphics types.
-- We don't need to know their structure, just how to pass them around.
data CFMachPort

data CFRunLoopSource

data CFRunLoop

data CGEvent

-- Define Haskell types for pointers to the opaque C types.
type CFMachPortRef = Ptr CFMachPort

type CFRunLoopSourceRef = Ptr CFRunLoopSource

type CFRunLoopRef = Ptr CFRunLoop

type CGEventRef = Ptr CGEvent

-- An alias for CGKeyCode, which is a CUShort (16-bit unsigned integer).
type CGKeyCode = CUShort

-- Define Haskell types for constants used in the C APIs.
type CGEventTapProxy = Ptr ()

type CGEventType = CUInt

type CGEventTapLocation = CUInt

type CGEventTapPlacement = CUInt

type CGEventTapOptions = CUInt

-- The event mask type, which must be an unsigned 64-bit integer.
type CGEventMask = CULong

-- Constants for CGEventTapCreate
kCGHeadInsertEventTap :: CGEventTapPlacement
kCGHeadInsertEventTap = 0

kCGDefaultEventTap :: CGEventTapOptions
kCGDefaultEventTap = 0

-- We are interested in key down and key up events.
kCGEventKeyDown :: CGEventType
kCGEventKeyDown = 10

kCGEventKeyUp :: CGEventType
kCGEventKeyUp = 11

-- This is the type of the Haskell callback function that will be called by macOS
-- every time a keyboard event occurs.
type EventCallback = CGEventTapProxy -> CGEventType -> CGEventRef -> Ptr () -> IO CGEventRef

-- We need a "wrapper" to allow a Haskell IO action to be called from C.
foreign import ccall "wrapper"
  mkEventCallback :: EventCallback -> IO (FunPtr EventCallback)

-- FFI imports for the C functions we need to call. Using 'safe' is more robust
-- for calls into stateful OS frameworks that interact with threads/event loops.

-- Creates an event tap.
-- The event mask parameter is now correctly typed as CGEventMask (CULong).
foreign import ccall safe "CGEventTapCreate"
  c_CGEventTapCreate :: CGEventTapLocation -> CGEventTapPlacement -> CGEventTapOptions -> CGEventMask -> FunPtr EventCallback -> Ptr () -> IO CFMachPortRef

-- Creates a run loop source from the event tap.
foreign import ccall safe "CFMachPortCreateRunLoopSource"
  c_CFMachPortCreateRunLoopSource :: Ptr () -> CFMachPortRef -> CLong -> IO CFRunLoopSourceRef

-- Gets the current thread's run loop.
foreign import ccall safe "CFRunLoopGetCurrent"
  c_CFRunLoopGetCurrent :: IO CFRunLoopRef

-- Adds a source to the run loop.
-- The C API allows the mode to be NULL, which uses the default mode.
foreign import ccall safe "CFRunLoopAddSource"
  c_CFRunLoopAddSource :: CFRunLoopRef -> CFRunLoopSourceRef -> Ptr CChar -> IO ()

-- Starts the run loop, which will block and wait for events.
foreign import ccall safe "CFRunLoopRun"
  c_CFRunLoopRun :: IO ()

-- Gets a specific field from a CGEvent, in this case the keycode.
foreign import ccall safe "CGEventGetIntegerValueField"
  c_CGEventGetIntegerValueField :: CGEventRef -> CInt -> IO CLong

-- A constant identifying the keycode field within an event.
kCGKeyboardEventKeycode :: CInt
kCGKeyboardEventKeycode = 9
