-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module SendControl where

--import Data.Maybe
import Foreign.C
import Data.Char( isUpper )
import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.UI.Gtk as GTK
import Control.Exception( bracket )
import Control.Concurrent( threadDelay )


-- ***FIX*** why is this missing?
-- it was in the docs on the web site
-- just Ubuntu not up to date?
-- remove this and convert noModMask to X.noModMask
noModMask :: X.KeyMask
noModMask = 0

eol :: X.KeySym
--eol = X.xK_Return
eol = X.xK_Linefeed


-- group together the low leve X related items
-- needed to send a key event
type NativeAccess = (X.Display, X.Window, X.Window)


-- send a list of keys
-- ***TODO*** allow shift, control
-- ***UNTESTED***
send :: GTK.Socket -> [String] -> IO ()
send socket keyList = withNative socket $ \native -> do
  mapM_ (\k -> sendKey native noModMask (symbol k)) keyList
  where
    symbol = X.stringToKeysym


-- send a line ended by newline
-- each character of the string is treated as a separate keysym
sendLine :: GTK.Socket -> String -> IO ()
sendLine socket str = withNative socket $ \native -> do
  mapM_ (\c -> do
            let (shift, symbol) = sym c
            sendKey native shift symbol
        )str
  sendKey native noModMask eol


-- bracket all the messy details of accessing Xlib
-- opens and closes the display, gets root window
-- finds the X window ID corresponding to the "plug" in the GTK socket
withNative :: GTK.Socket -> (NativeAccess -> IO ()) -> IO ()
withNative socket run =
  bracket setup final run
  where
    setup :: IO NativeAccess
    setup = do
      putStrLn $ "setup"
      plugWindow <-  GTK.socketGetPlugWindow socket
      window <- GTK.drawableGetID plugWindow
      let nativeWindow = GTK.fromNativeWindowId window
      display <- X.openDisplay ""
      let root = X.defaultRootWindow display
      return (display, root, nativeWindow)
    final :: NativeAccess -> IO ()
    final (display, _root, _window) = do
      putStrLn $ "final"
      X.closeDisplay display


-- send the key event
-- needs flush and delay to ensure that the event actually gets sent
-- delay appears to be required or the event queue is overloaded
-- and the urxvt ceases to respond to normal key presses
sendKey ::  NativeAccess -> X.KeyMask -> X.KeySym -> IO ()
sendKey (display, root, window) shift keysym = do
  keycode <- X.keysymToKeycode display keysym
  X.allocaXEvent $ \ke -> do
    XE.setEventType ke X.keyPress
    XE.setKeyEvent ke window root XE.none shift keycode True
    X.sendEvent display window True X.keyPressMask ke
    XE.setEventType ke X.keyRelease
    X.sendEvent display window True X.keyReleaseMask ke
  X.flush display  -- ensure the key is sent immediately
  threadDelay 100 -- must delay otherwise the event queue fails

{-
  mapM_ (\n -> do
            let k = X.keysymToString (n :: X.KeySym)
            let c = fromIntegral n ::Int
            putStrLn $ "SYM: " ++ (show (chr c)) ++ " -> " ++ (show k)
        ) $ take 256 [32..]
  exitFailure
-}


-- convert ASCII character to the correct key
sym :: Char -> (X.KeyMask, X.KeySym)
sym ' ' = (noModMask, X.stringToKeysym "space")
sym '!' = (X.shiftMask, X.stringToKeysym "exclam")
sym '"' = (X.shiftMask, X.stringToKeysym "quotedbl")
sym '#' = (X.shiftMask, X.stringToKeysym "numbersign")
sym '$' = (X.shiftMask, X.stringToKeysym "dollar")
sym '%' = (X.shiftMask, X.stringToKeysym "percent")
sym '&' = (X.shiftMask, X.stringToKeysym "ampersand")
sym '\'' = (noModMask, X.stringToKeysym "apostrophe")
sym '(' = (X.shiftMask, X.stringToKeysym "parenleft")
sym ')' = (X.shiftMask, X.stringToKeysym "parenright")
sym '*' = (X.shiftMask, X.stringToKeysym "asterisk")
sym '+' = (X.shiftMask, X.stringToKeysym "plus")
sym ',' = (noModMask, X.stringToKeysym "comma")
sym '-' = (noModMask, X.stringToKeysym "minus")
sym '.' = (noModMask, X.stringToKeysym "period")
sym '/' = (noModMask, X.stringToKeysym "slash")
sym '0' = (noModMask, X.stringToKeysym "0")
sym '1' = (noModMask, X.stringToKeysym "1")
sym '2' = (noModMask, X.stringToKeysym "2")
sym '3' = (noModMask, X.stringToKeysym "3")
sym '4' = (noModMask, X.stringToKeysym "4")
sym '5' = (noModMask, X.stringToKeysym "5")
sym '6' = (noModMask, X.stringToKeysym "6")
sym '7' = (noModMask, X.stringToKeysym "7")
sym '8' = (noModMask, X.stringToKeysym "8")
sym '9' = (noModMask, X.stringToKeysym "9")
sym ':' = (X.shiftMask, X.stringToKeysym "colon")
sym ';' = (noModMask, X.stringToKeysym "semicolon")
sym '<' = (X.shiftMask, X.stringToKeysym "less")
sym '=' = (noModMask, X.stringToKeysym "equal")
sym '>' = (X.shiftMask, X.stringToKeysym "greater")
sym '?' = (X.shiftMask, X.stringToKeysym "question")
sym '@' = (X.shiftMask, X.stringToKeysym "at")
sym 'A' = (noModMask, X.stringToKeysym "A")
sym 'B' = (noModMask, X.stringToKeysym "B")
sym 'C' = (noModMask, X.stringToKeysym "C")
sym 'D' = (noModMask, X.stringToKeysym "D")
sym 'E' = (noModMask, X.stringToKeysym "E")
sym 'F' = (noModMask, X.stringToKeysym "F")
sym 'G' = (noModMask, X.stringToKeysym "G")
sym 'H' = (noModMask, X.stringToKeysym "H")
sym 'I' = (noModMask, X.stringToKeysym "I")
sym 'J' = (noModMask, X.stringToKeysym "J")
sym 'K' = (noModMask, X.stringToKeysym "K")
sym 'L' = (noModMask, X.stringToKeysym "L")
sym 'M' = (noModMask, X.stringToKeysym "M")
sym 'N' = (noModMask, X.stringToKeysym "N")
sym 'O' = (noModMask, X.stringToKeysym "O")
sym 'P' = (noModMask, X.stringToKeysym "P")
sym 'Q' = (noModMask, X.stringToKeysym "Q")
sym 'R' = (noModMask, X.stringToKeysym "R")
sym 'S' = (noModMask, X.stringToKeysym "S")
sym 'T' = (noModMask, X.stringToKeysym "T")
sym 'U' = (noModMask, X.stringToKeysym "U")
sym 'V' = (noModMask, X.stringToKeysym "V")
sym 'W' = (noModMask, X.stringToKeysym "W")
sym 'X' = (noModMask, X.stringToKeysym "X")
sym 'Y' = (noModMask, X.stringToKeysym "Y")
sym 'Z' = (noModMask, X.stringToKeysym "Z")
sym '[' = (noModMask, X.stringToKeysym "bracketleft")
sym '\\' =(noModMask, X.stringToKeysym  "backslash")
sym ']' = (noModMask, X.stringToKeysym "bracketright")
sym '^' = (X.shiftMask, X.stringToKeysym "asciicircum")
sym '_' = (X.shiftMask, X.stringToKeysym "underscore")
sym '`' = (noModMask, X.stringToKeysym "grave")
sym 'a' = (noModMask, X.stringToKeysym "a")
sym 'b' = (noModMask, X.stringToKeysym "b")
sym 'c' = (noModMask, X.stringToKeysym "c")
sym 'd' = (noModMask, X.stringToKeysym "d")
sym 'e' = (noModMask, X.stringToKeysym "e")
sym 'f' = (noModMask, X.stringToKeysym "f")
sym 'g' = (noModMask, X.stringToKeysym "g")
sym 'h' = (noModMask, X.stringToKeysym "h")
sym 'i' = (noModMask, X.stringToKeysym "i")
sym 'j' = (noModMask, X.stringToKeysym "j")
sym 'k' = (noModMask, X.stringToKeysym "k")
sym 'l' = (noModMask, X.stringToKeysym "l")
sym 'm' = (noModMask, X.stringToKeysym "m")
sym 'n' = (noModMask, X.stringToKeysym "n")
sym 'o' = (noModMask, X.stringToKeysym "o")
sym 'p' = (noModMask, X.stringToKeysym "p")
sym 'q' = (noModMask, X.stringToKeysym "q")
sym 'r' = (noModMask, X.stringToKeysym "r")
sym 's' = (noModMask, X.stringToKeysym "s")
sym 't' = (noModMask, X.stringToKeysym "t")
sym 'u' = (noModMask, X.stringToKeysym "u")
sym 'v' = (noModMask, X.stringToKeysym "v")
sym 'w' = (noModMask, X.stringToKeysym "w")
sym 'x' = (noModMask, X.stringToKeysym "x")
sym 'y' = (noModMask, X.stringToKeysym "y")
sym 'z' = (noModMask, X.stringToKeysym "z")
sym '{' = (X.shiftMask, X.stringToKeysym "braceleft")
sym '|' = (X.shiftMask, X.stringToKeysym "bar")
sym '}' = (X.shiftMask, X.stringToKeysym "braceright")
sym '~' = (X.shiftMask, X.stringToKeysym "asciitilde")
-- unsupported codes just convert to space
sym _ = (noModMask, X.stringToKeysym "space")
