-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module TerminalUI where

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
            addPane notebook title dir command) tabList

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
  page <- GTK.notebookGetCurrentPage notebook
  let continue = page /= -1
  if continue then exitNotice else GTK.mainQuit
  return $ continue

exitNotice = do
  dialog <- GTK.messageDialogNew Nothing [GTK.DialogDestroyWithParent] GTK.MessageWarning GTK.ButtonsOk "Some tabs are still active"
  response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog
  putStrLn $ "exit " ++ (show response)


addPane :: GTK.Notebook ->  String -> Maybe String -> CP.CommandList -> IO Int
addPane notebook title dir commandList = do

  socket <- GTK.socketNew
  GTK.widgetShowAll socket
  GTK.widgetSetCanFocus socket True

  page <- GTK.notebookAppendPage notebook socket title

  paneid <- GTK.socketGetId socket
  let windowID = GTK.fromNativeWindowId paneid :: Integer
  let run = CP.expandCommand commandList windowID title

  GTK.on socket GTK.socketPlugRemoved $ unplug socket page dir run
  GTK.on socket GTK.socketPlugAdded $ plug socket

  putStrLn $ "RUN: " ++ (show run)
  PR.run dir run

  return page


-- detect the program creating its main window
-- delay inorder to give it tiome to set itself up
-- send too quickly and the event queue locks up
plug :: GTK.Socket -> IO ()
plug socket = do
  putStrLn $ "plug "
  h <- GTK.timeoutAdd (delayedSend socket >> return False) 1000
  return ()


-- dummy routine to send a couple of test lines
delayedSend socket = do
  putStrLn "plug--delay"

  SC.sendLine socket "ls"
  SC.sendLine socket "echo the quick brown fox jumps over the lazy dog"


-- dialog to decide whether to restart the command
unplug :: GTK.Socket -> Int -> Maybe String -> [String] -> IO Bool
unplug socket page dir run = do
  putStrLn $ "unplug "
  dialog <- GTK.dialogNew

  GTK.dialogAddButton dialog "Restart" GTK.ResponseOk
  GTK.dialogAddButton dialog "Close" GTK.ResponseClose
  GTK.dialogSetDefaultResponse dialog GTK.ResponseOk
  response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog
  putStrLn $ "unplug " ++ (show response)
  case response of
     GTK.ResponseOk -> do
       PR.run dir run
       return True
     _ ->
       return False


-- change the main title to be the tab name
pageChange :: GTK.Window -> GTK.Notebook -> Int -> IO ()
pageChange window notebook page = do
  child <- GTK.notebookGetNthPage notebook page
  title <- case child of
    Nothing -> return "Terminal"
    Just child -> do
      GTK.widgetSetCanFocus child True
      GTK.widgetGrabFocus child
      text <- GTK.notebookGetTabLabelText notebook child
      case text of
        Nothing -> return "Terminal"
        Just title -> return title
  putStrLn $ "page: " ++  (show page) ++ " title: " ++ title
  GTK.set window [GTK.windowTitle GTK.:= title]
