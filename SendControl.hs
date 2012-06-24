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
  mapM_ (\k -> sendKey native noModMask (sym k)) keyList
  where
    sym = X.stringToKeysym


-- send a line ended by newline
-- each character of the string is treated as a separate keysym
sendLine :: GTK.Socket -> String -> IO ()
sendLine socket str = withNative socket $ \native -> do
  mapM_ (\ch -> sendKey native (shift ch) (sym ch)) str
  sendKey native noModMask eol
  where
    shift ch = if isUpper ch then X.shiftMask else noModMask
    sym ' ' = X.stringToKeysym "space"
    sym ch = X.stringToKeysym [ch]


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
