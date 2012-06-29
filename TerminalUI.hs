-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module TerminalUI where

import Data.Maybe( fromJust )
import Data.Foldable( foldlM )
import Control.Monad.Trans( liftIO )
import System.Process( ProcessHandle )

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

  -- create the buttons page
  table <- GTK.tableNew 4 4 True
  GTK.widgetShowAll table
  page <- GTK.notebookAppendPage notebook table "+NEW"


  -- create all the initial table
  mapM_ (\tab ->  do
            let (title, start, dir, command, sendList) = tab
            addPane notebook title start dir command sendList) tabList

  -- create buttons
  foldlM (\(x, y) button ->  do
            let (title, start, dir, command, sendList) = button
            addButton table x y notebook title start dir command sendList
            let x1 = x + 1
            if x > 4 then return (0, y + 1) else return (x1, y)
         ) (0, 0) buttonList

  -- link up the close button
  GTK.on toplevel GTK.deleteEvent $ liftIO $ checkExit

  -- start the GTK event loop
  GTK.mainGUI


-- do not allow exit if still some tabs are open
checkExit :: IO Bool
checkExit = do
  active <- PR.activeProcs
  let continue = active > 0
  if continue then exitNotice else GTK.mainQuit
  return $ continue


exitNotice = do
  dialog <- GTK.messageDialogNew Nothing [GTK.DialogDestroyWithParent] GTK.MessageWarning GTK.ButtonsOk "Some tabs are still active"
  response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog


-- add buttons to the button menu
addButton :: GTK.Table -> Int -> Int -> GTK.Notebook -> String -> Bool -> Maybe String -> CP.CommandList -> [String] -> IO ()
addButton table x y notebook title autoStart dir commandList sendList = do
  button <- GTK.buttonNewWithLabel title
  GTK.on button GTK.buttonActivated $ (addPane notebook title autoStart dir commandList sendList >> return ())

  GTK.widgetShowAll button
  GTK.tableAttachDefaults table button x (x + 1) y (y + 1)
  return ()


-- add auto/manual stared panes
addPane :: GTK.Notebook ->  String -> Bool -> Maybe String -> CP.CommandList -> [String] -> IO Int
addPane notebook title autoStart dir commandList sendList = do
  vbox <- GTK.vBoxNew False 0
  GTK.widgetSetCanFocus vbox False

  GTK.widgetShowAll vbox

  -- create a socket and put it in the Vbox
  socket <- GTK.socketNew
  GTK.widgetSetCanFocus socket True
  GTK.containerAdd vbox socket

  -- to hiold the process that wil be started later
  refproc <- PR.newProcRef

  -- start
  sb <- GTK.buttonNewWithLabel "Start"
  GTK.on sb GTK.buttonActivated $ press sb socket title refproc dir commandList
  GTK.containerAdd vbox sb

  if autoStart
    then return ()
    else GTK.widgetShowAll sb


  page <- GTK.notebookAppendPage notebook vbox title

  GTK.on socket GTK.socketPlugRemoved $ unplug sb socket refproc
  GTK.on socket GTK.socketPlugAdded $ plug socket sendList

  if autoStart
    then do
      runC refproc socket title dir commandList
    else return ()

  return page


-- run a command
runC :: PR.ProcRef -> GTK.Socket -> String -> Maybe String -> CP.CommandList -> IO ()
runC refproc socket title dir commandList = do
  GTK.widgetShowAll socket

  -- expand the command string
  paneid <- GTK.socketGetId socket
  let windowID = GTK.fromNativeWindowId paneid :: Integer
  let cmd = CP.expandCommand commandList windowID title
  PR.run refproc dir cmd


-- button pressed
press :: GTK.Button -> GTK.Socket -> String -> PR.ProcRef -> Maybe String -> CP.CommandList -> IO ()
press button socket title refproc dir commandList = do
  GTK.widgetHide button
  runC refproc socket title dir commandList
  GTK.widgetGrabFocus socket


-- detect the program creating its main window
-- delay in order to give it time to set itself up
-- send too quickly and the event queue locks up
plug :: GTK.Socket -> [String] -> IO ()
plug socket sendList = do
  h <- GTK.timeoutAdd (delayedSend socket sendList) 1000
  return ()


-- dummy routine to send a couple of test lines
delayedSend :: GTK.Socket -> [String] -> IO Bool
delayedSend socket sendList = do
  mapM_ (SC.sendLine socket) sendList
  return False


-- dialog to decide whether to restart the command
unplug :: GTK.Button -> GTK.Socket ->  PR.ProcRef -> IO Bool
unplug button socket refproc = do
  GTK.widgetHide socket
  GTK.buttonSetLabel button "Restart"

  PR.shutdown refproc

  GTK.widgetShowAll button

  return True


-- change the main title to be the tab name
pageChange :: GTK.Window -> GTK.Notebook -> Int -> IO ()
pageChange window notebook page = do
  if page == 0 then return () else xpageChange window notebook page

xpageChange window notebook page = do
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
  GTK.set window [GTK.windowTitle GTK.:= title]
