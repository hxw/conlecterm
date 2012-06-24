-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module TerminalUI where

import Data.Maybe( fromJust )
import Control.Monad.Trans( liftIO )

import qualified Graphics.UI.Gtk as GTK

import qualified Text.Read as TR

import qualified ConfigurationParser as CP
import qualified ProcessRunner as PR
import qualified SendControl as SC


orientation :: CP.Orientation -> GTK.PositionType
orientation CP.LeftTabs   = GTK.PosLeft
orientation CP.RightTabs  = GTK.PosRight
orientation CP.TopTabs    = GTK.PosTop
orientation CP.BottomTabs = GTK.PosBottom


run :: CP.SessionInfo -> IO ()
run (orient, tabList, buttonList) = do
  GTK.initGUI
  toplevel <- GTK.windowNew
  notebook <- GTK.notebookNew
  GTK.notebookSetTabPos notebook $ orientation orient
  GTK.notebookSetHomogeneousTabs notebook True

  GTK.windowSetDefaultSize toplevel 800 600
  GTK.set toplevel [GTK.windowTitle GTK.:= "Terminal"]

  toplevel `GTK.containerAdd` notebook
  GTK.widgetShowAll toplevel

  GTK.on notebook GTK.switchPage $ pageChange toplevel notebook
  GTK.widgetSetCanFocus notebook False

  -- create all the initial table
  mapM_ (\tab ->  do
            let (title, start, dir, command, sendList) = tab
            addPane notebook title start dir command sendList) tabList

  -- create buttons
  -- ***TODO*** ?? where to put?  on a special tab?
  -- ????

  -- link up the close button
  GTK.on toplevel GTK.deleteEvent $ liftIO $ checkExit notebook

  -- start the GTK event loop
  GTK.mainGUI


-- do not allow exit if still some tabs are open
checkExit :: GTK.Notebook -> IO Bool
checkExit notebook = do
  {-
  page <- GTK.notebookGetCurrentPage notebook
  let continue = page /= -1
-}
  let continue = False
  if continue then exitNotice else GTK.mainQuit
  return $ continue

exitNotice = do
  dialog <- GTK.messageDialogNew Nothing [GTK.DialogDestroyWithParent] GTK.MessageWarning GTK.ButtonsOk "Some tabs are still active"
  response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog
  putStrLn $ "exit " ++ (show response)


addPane :: GTK.Notebook ->  String -> Bool -> Maybe String -> CP.CommandList -> [String] -> IO Int
addPane notebook title autoStart dir commandList sendList = do
  putStrLn $ "send: " ++ (show sendList)
  vbox <- GTK.vBoxNew False 0
  GTK.widgetSetCanFocus vbox False

  GTK.widgetShowAll vbox

  -- create a socket and put it in the Vbox
  socket <- GTK.socketNew
  GTK.widgetSetCanFocus socket True
  GTK.containerAdd vbox socket

  -- start
  sb <- GTK.buttonNewWithLabel "Start"
  GTK.on sb GTK.buttonActivated $ press sb socket title dir commandList
  GTK.containerAdd vbox sb

  if autoStart
    then return ()
    else GTK.widgetShowAll sb


  page <- GTK.notebookAppendPage notebook vbox title


  GTK.on socket GTK.socketPlugRemoved $ unplug sb socket
  GTK.on socket GTK.socketPlugAdded $ plug socket sendList

  if autoStart
    then runC socket title dir commandList
    else return ()

  return page


-- run a command
runC :: GTK.Socket -> String -> Maybe String -> CP.CommandList -> IO ()
runC socket title dir commandList = do
  GTK.widgetShowAll socket

  -- expand the command string
  paneid <- GTK.socketGetId socket
  let windowID = GTK.fromNativeWindowId paneid :: Integer
  let cmd = CP.expandCommand commandList windowID title
  putStrLn $ "RUN: " ++ (show cmd)
  PR.run dir cmd


-- button pressed
press :: GTK.Button -> GTK.Socket -> String -> Maybe String -> CP.CommandList -> IO ()
press button socket title dir commandList = do
  GTK.widgetHide button
  putStrLn $ "press "
  runC socket title dir commandList
  GTK.widgetGrabFocus socket


-- detect the program creating its main window
-- delay in order to give it time to set itself up
-- send too quickly and the event queue locks up
plug :: GTK.Socket -> [String] -> IO ()
plug socket sendList = do
  putStrLn $ "plug "
  h <- GTK.timeoutAdd (delayedSend socket sendList) 1000
  return ()


-- dummy routine to send a couple of test lines
delayedSend :: GTK.Socket -> [String] -> IO Bool
delayedSend socket sendList = do
  putStrLn "plug--delay"
  mapM_ (SC.sendLine socket) sendList
  return False


-- dialog to decide whether to restart the command
unplug :: GTK.Button -> GTK.Socket -> IO Bool
unplug button socket = do
  putStrLn $ "unplug "
  GTK.widgetHide socket
  GTK.buttonSetLabel button "Restart"
  GTK.widgetShowAll button

  return True


-- change the main title to be the tab name
pageChange :: GTK.Window -> GTK.Notebook -> Int -> IO ()
pageChange window notebook page = do
  vBox <- GTK.notebookGetNthPage notebook page
  children <- GTK.containerGetChildren $ GTK.castToVBox $ fromJust vBox

  title <- case children of
    [] -> return "Terminal"
    (child:_) -> do
      GTK.widgetSetCanFocus child True
      GTK.widgetGrabFocus child
      text <- GTK.notebookGetTabLabelText notebook child
      case text of
        Nothing -> return "Terminal"
        Just title -> return title
  putStrLn $ "page: " ++  (show page) ++ " title: " ++ title
  GTK.set window [GTK.windowTitle GTK.:= title]
