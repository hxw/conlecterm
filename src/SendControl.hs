-- Copyright (c) 2012-2019, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

module SendControl where

import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.UI.Gtk as GTK

import Control.Exception( bracket )
import Control.Concurrent( threadDelay )


-- the end-of-line symbol
eol :: X.KeySym
eol = X.xK_Return
--eol = X.xK_Linefeed

-- delay value
sendDelayMicroseconds :: Int
sendDelayMicroseconds = 5000

-- group together the low level X related items
-- needed to send a key event
type NativeAccess = (X.Display, X.Window, X.Window)


-- send a list of keys
-- ***TODO*** allow shift, control
-- ***UNTESTED***
send :: GTK.Socket -> [String] -> IO ()
send socket keyList = withNative socket $ \native -> do
  mapM_ (\k -> sendOneKey native X.noModMask (symbol k)) keyList
  where
    symbol = X.stringToKeysym

-- send a single of keysym
sendKey :: GTK.Socket -> [GTK.Modifier] -> GTK.KeyVal -> IO ()
sendKey socket mods key = withNative socket $ \native -> do
  let k1 = (fromIntegral key) :: Word
  let k = (fromIntegral k1) :: X.KeySym
  let modMask = foldl makeMask X.noModMask mods
  sendOneKey native modMask k
      where
        makeMask a k = a + (modToMask k)

-- send a line ended by newline
-- each character of the string is treated as a separate keysym
sendLine :: GTK.Socket -> String -> IO ()
sendLine socket str = withNative socket $ \native -> do
  mapM_ (\c -> do
            let (shift, symbol) = sym c
            sendOneKey native shift symbol
        )str
  sendOneKey native X.noModMask eol


-- bracket all the messy details of accessing Xlib
-- opens and closes the display, gets root window
-- finds the X window ID corresponding to the "plug" in the GTK socket
withNative :: GTK.Socket -> (NativeAccess -> IO ()) -> IO ()
withNative socket run =
  bracket setup final run
  where
    setup :: IO NativeAccess
    setup = do
      socketId <- GTK.socketGetId socket
      display <- X.openDisplay ""
      --let root = X.defaultRootWindow display
      let nativeSkt = GTK.fromNativeWindowId socketId :: X.Window
      -- appears to return (root, parent, [children]) and Plug is first child
      (root, _parent, plugId:_) <- XE.queryTree display nativeSkt
      return (display, root, plugId)
    final :: NativeAccess -> IO ()
    final (display, _root, _window) = do
      X.closeDisplay display


-- send the key event
-- needs flush and delay to ensure that the event actually gets sent
-- delay appears to be required or the event queue is overloaded
-- and the urxvt ceases to respond to normal key presses
sendOneKey ::  NativeAccess -> X.KeyMask -> X.KeySym -> IO ()
sendOneKey (display, root, window) shift keysym = do
  keycode <- X.keysymToKeycode display keysym
  X.allocaXEvent $ \ke -> do
    XE.setEventType ke X.keyPress
    XE.setKeyEvent ke window root XE.none shift keycode True
    X.sendEvent display window True X.keyPressMask ke
    XE.setEventType ke X.keyRelease
    X.sendEvent display window True X.keyReleaseMask ke
  X.flush display  -- ensure the key is sent immediately
  threadDelay sendDelayMicroseconds -- must delay otherwise the event queue fails

{-
  mapM_ (\n -> do
            let k = X.keysymToString (n :: X.KeySym)
            let c = fromIntegral n ::Int
            putStrLn $ "SYM: " ++ (show (chr c)) ++ " -> " ++ (show k)
        ) $ take 256 [32..]
  exitFailure
-}


-- convert ASCII character to the correct key
-- unfortunately since key codes are sent the
-- corresponding shift is also needed
-- e.g. the key codes for 2 and @ are the same
-- this code probably is tied to the US-international layout
sym :: Char -> (X.KeyMask, X.KeySym)
sym ' ' = (X.noModMask, X.stringToKeysym "space")
sym '!' = (X.shiftMask, X.stringToKeysym "exclam")
sym '"' = (X.shiftMask, X.stringToKeysym "quotedbl")
sym '#' = (X.shiftMask, X.stringToKeysym "numbersign")
sym '$' = (X.shiftMask, X.stringToKeysym "dollar")
sym '%' = (X.shiftMask, X.stringToKeysym "percent")
sym '&' = (X.shiftMask, X.stringToKeysym "ampersand")
sym '\'' = (X.noModMask, X.stringToKeysym "apostrophe")
sym '(' = (X.shiftMask, X.stringToKeysym "parenleft")
sym ')' = (X.shiftMask, X.stringToKeysym "parenright")
sym '*' = (X.shiftMask, X.stringToKeysym "asterisk")
sym '+' = (X.shiftMask, X.stringToKeysym "plus")
sym ',' = (X.noModMask, X.stringToKeysym "comma")
sym '-' = (X.noModMask, X.stringToKeysym "minus")
sym '.' = (X.noModMask, X.stringToKeysym "period")
sym '/' = (X.noModMask, X.stringToKeysym "slash")
sym '0' = (X.noModMask, X.stringToKeysym "0")
sym '1' = (X.noModMask, X.stringToKeysym "1")
sym '2' = (X.noModMask, X.stringToKeysym "2")
sym '3' = (X.noModMask, X.stringToKeysym "3")
sym '4' = (X.noModMask, X.stringToKeysym "4")
sym '5' = (X.noModMask, X.stringToKeysym "5")
sym '6' = (X.noModMask, X.stringToKeysym "6")
sym '7' = (X.noModMask, X.stringToKeysym "7")
sym '8' = (X.noModMask, X.stringToKeysym "8")
sym '9' = (X.noModMask, X.stringToKeysym "9")
sym ':' = (X.shiftMask, X.stringToKeysym "colon")
sym ';' = (X.noModMask, X.stringToKeysym "semicolon")
sym '<' = (X.noModMask, X.stringToKeysym "less")
sym '=' = (X.noModMask, X.stringToKeysym "equal")
sym '>' = (X.shiftMask, X.stringToKeysym "greater")
sym '?' = (X.shiftMask, X.stringToKeysym "question")
sym '@' = (X.shiftMask, X.stringToKeysym "at")
sym 'A' = (X.shiftMask, X.stringToKeysym "A")
sym 'B' = (X.shiftMask, X.stringToKeysym "B")
sym 'C' = (X.shiftMask, X.stringToKeysym "C")
sym 'D' = (X.shiftMask, X.stringToKeysym "D")
sym 'E' = (X.shiftMask, X.stringToKeysym "E")
sym 'F' = (X.shiftMask, X.stringToKeysym "F")
sym 'G' = (X.shiftMask, X.stringToKeysym "G")
sym 'H' = (X.shiftMask, X.stringToKeysym "H")
sym 'I' = (X.shiftMask, X.stringToKeysym "I")
sym 'J' = (X.shiftMask, X.stringToKeysym "J")
sym 'K' = (X.shiftMask, X.stringToKeysym "K")
sym 'L' = (X.shiftMask, X.stringToKeysym "L")
sym 'M' = (X.shiftMask, X.stringToKeysym "M")
sym 'N' = (X.shiftMask, X.stringToKeysym "N")
sym 'O' = (X.shiftMask, X.stringToKeysym "O")
sym 'P' = (X.shiftMask, X.stringToKeysym "P")
sym 'Q' = (X.shiftMask, X.stringToKeysym "Q")
sym 'R' = (X.shiftMask, X.stringToKeysym "R")
sym 'S' = (X.shiftMask, X.stringToKeysym "S")
sym 'T' = (X.shiftMask, X.stringToKeysym "T")
sym 'U' = (X.shiftMask, X.stringToKeysym "U")
sym 'V' = (X.shiftMask, X.stringToKeysym "V")
sym 'W' = (X.shiftMask, X.stringToKeysym "W")
sym 'X' = (X.shiftMask, X.stringToKeysym "X")
sym 'Y' = (X.shiftMask, X.stringToKeysym "Y")
sym 'Z' = (X.shiftMask, X.stringToKeysym "Z")
sym '[' = (X.noModMask, X.stringToKeysym "bracketleft")
sym '\\' =(X.noModMask, X.stringToKeysym  "backslash")
sym ']' = (X.noModMask, X.stringToKeysym "bracketright")
sym '^' = (X.shiftMask, X.stringToKeysym "asciicircum")
sym '_' = (X.shiftMask, X.stringToKeysym "underscore")
sym '`' = (X.noModMask, X.stringToKeysym "grave")
sym 'a' = (X.noModMask, X.stringToKeysym "a")
sym 'b' = (X.noModMask, X.stringToKeysym "b")
sym 'c' = (X.noModMask, X.stringToKeysym "c")
sym 'd' = (X.noModMask, X.stringToKeysym "d")
sym 'e' = (X.noModMask, X.stringToKeysym "e")
sym 'f' = (X.noModMask, X.stringToKeysym "f")
sym 'g' = (X.noModMask, X.stringToKeysym "g")
sym 'h' = (X.noModMask, X.stringToKeysym "h")
sym 'i' = (X.noModMask, X.stringToKeysym "i")
sym 'j' = (X.noModMask, X.stringToKeysym "j")
sym 'k' = (X.noModMask, X.stringToKeysym "k")
sym 'l' = (X.noModMask, X.stringToKeysym "l")
sym 'm' = (X.noModMask, X.stringToKeysym "m")
sym 'n' = (X.noModMask, X.stringToKeysym "n")
sym 'o' = (X.noModMask, X.stringToKeysym "o")
sym 'p' = (X.noModMask, X.stringToKeysym "p")
sym 'q' = (X.noModMask, X.stringToKeysym "q")
sym 'r' = (X.noModMask, X.stringToKeysym "r")
sym 's' = (X.noModMask, X.stringToKeysym "s")
sym 't' = (X.noModMask, X.stringToKeysym "t")
sym 'u' = (X.noModMask, X.stringToKeysym "u")
sym 'v' = (X.noModMask, X.stringToKeysym "v")
sym 'w' = (X.noModMask, X.stringToKeysym "w")
sym 'x' = (X.noModMask, X.stringToKeysym "x")
sym 'y' = (X.noModMask, X.stringToKeysym "y")
sym 'z' = (X.noModMask, X.stringToKeysym "z")
sym '{' = (X.shiftMask, X.stringToKeysym "braceleft")
sym '|' = (X.shiftMask, X.stringToKeysym "bar")
sym '}' = (X.shiftMask, X.stringToKeysym "braceright")
sym '~' = (X.shiftMask, X.stringToKeysym "asciitilde")
-- unsupported codes just convert to space
sym _ = (X.noModMask, X.stringToKeysym "space")


-- convert a modifier to a mask
modToMask :: GTK.Modifier -> X.KeyMask
modToMask GTK.Shift = X.shiftMask
modToMask GTK.Lock = X.lockMask
modToMask GTK.Control = X.controlMask
modToMask GTK.Alt = X.mod1Mask
modToMask GTK.Alt2 = X.mod2Mask
modToMask GTK.Alt3 = X.mod3Mask
modToMask GTK.Alt4 = X.mod4Mask
modToMask GTK.Alt5 = X.mod5Mask
--modToMask GTK.Button1 = 0
--modToMask GTK.Button2 = 0
--modToMask GTK.Button3 = 0
--modToMask GTK.Button4 = 0
--modToMask GTK.Button5 = 0
--modToMask GTK.MODIFIER_RESERVED_13_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_14_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_15_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_16_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_17_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_18_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_19_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_20_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_21_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_22_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_23_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_24_MASK = 0
--modToMask GTK.MODIFIER_RESERVED_25_MASK = 0
--modToMask GTK.Super = 0
--modToMask GTK.Hyper = 0
--modToMask GTK.Meta = 0
--modToMask GTK.MODIFIER_RESERVED_29_MASK = 0
--modToMask GTK.Release = 0
--modToMask GTK.ModifierMask = 0
-- default
modToMask _ = X.noModMask
