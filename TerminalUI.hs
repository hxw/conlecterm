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

orientationToText :: CP.Orientation -> String
orientationToText CP.LeftTabs   = "left"
orientationToText CP.RightTabs  = "right"
orientationToText CP.TopTabs    = "top"
orientationToText CP.BottomTabs = "bottom"

run :: CP.SessionInfo -> IO ()
run (orient, tabList, buttonList) = do
  GTK.initGUI
  toplevel <- GTK.windowNew
  notebook <- GTK.notebookNew

  GTK.on notebook GTK.pageReordered $ reordered notebook

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
            let (name, title, start, dir, command, sendList, running, stopped) = tab
            addPane notebook name title start dir command sendList running stopped) tabList

  -- create buttons
  foldlM (\(x, y) button ->  do
            let (name, title, start, dir, command, sendList, running, stopped) = button
            addButton table x y notebook name title start dir command sendList running stopped
            let x1 = x + 1
            if x > 4 then return (0, y + 1) else return (x1, y)
         ) (0, 0) buttonList

  -- link up the close button
  GTK.on toplevel GTK.deleteEvent $ liftIO $ checkExit "default" orient notebook

  -- start the GTK event loop
  GTK.mainGUI


-- do not allow exit if still some tabs are open
checkExit :: String ->  CP.Orientation -> GTK.Notebook -> IO Bool
checkExit session orient notebook = do
  putStrLn $ "session \"" ++ session ++ "\" " ++ (orientationToText orient) ++ " {"
  pageCount <- GTK.notebookGetNPages notebook
  putStrLn $ "    # total tabs = " ++ (show pageCount)
  -- output current tabs
  let oneTab i = do
        p <- GTK.notebookGetNthPage notebook i
        case p of
          Nothing -> return ()
          Just page -> do
            name <- GTK.widgetGetName page
            putStrLn $ "    tab " ++ name
    in mapM_ oneTab [1 .. pageCount - 1]

  -- output current buttons
  buttonsPage <- GTK.notebookGetNthPage notebook 0
  case buttonsPage of
    Nothing -> return ()
    Just t -> do
      let table = GTK.castToTable t
      c <- GTK.containerGetChildren table
      -- the list c appears to be reversed!
      let pp w = do
            let button = GTK.castToButton w
            name <- GTK.widgetGetName button
            putStrLn $ "    button " ++ name
        in mapM_ pp $ reverse c

  putStrLn $ "}"
  active <- PR.activeProcs
  let continue = active > 0
  if continue then exitNotice else GTK.mainQuit
  return $ continue


-- dialog warning about acive tabs
exitNotice :: IO ()
exitNotice = do
  dialog <- GTK.messageDialogNew Nothing [GTK.DialogDestroyWithParent] GTK.MessageWarning GTK.ButtonsOk "Some tabs are still active"
  response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog


-- add buttons to the button menu
addButton :: GTK.Table -> Int -> Int -> GTK.Notebook -> String -> String -> Bool -> Maybe String -> CP.CommandList -> [String] -> Maybe GTK.Color -> Maybe GTK.Color -> IO ()
addButton table x y notebook name title autoStart dir commandList sendList running stopped = do
  label <- GTK.labelNew $ Just title

  case running of
    Nothing -> return ()
    Just colour -> do
      GTK.widgetModifyFg label GTK.StateNormal colour
      GTK.widgetModifyFg label GTK.StatePrelight colour
      GTK.widgetModifyFg label GTK.StateActive colour
  button <- GTK.buttonNew
  --GTK.widgetModifyBg button GTK.StateNormal (GTK.Color 32767 32757 32767)
  --GTK.widgetModifyBg button GTK.StatePrelight (GTK.Color 8191 8191 16383)
  --GTK.widgetModifyBg button GTK.StateActive (GTK.Color 0 0 0)

  GTK.widgetSetName button name
  GTK.containerAdd button label

  GTK.on button GTK.buttonActivated $ (addPane notebook name title autoStart dir commandList sendList running stopped >> return ())

  GTK.widgetShowAll button
  GTK.tableAttachDefaults table button x (x + 1) y (y + 1)
  return ()


-- add auto/manual stared panes
addPane :: GTK.Notebook ->  String -> String -> Bool -> Maybe String -> CP.CommandList -> [String] -> Maybe GTK.Color -> Maybe GTK.Color -> IO Int
addPane notebook name title autoStart dir commandList sendList running stopped = do
  vbox <- GTK.vBoxNew False 0
  GTK.widgetSetCanFocus vbox False
  GTK.widgetSetName vbox name

  GTK.widgetShowAll vbox

  -- create a socket and put it in the Vbox
  socket <- GTK.socketNew
  GTK.widgetSetCanFocus socket True
  GTK.containerAdd vbox socket

  -- to hold the process that will be started later
  refproc <- PR.newProcRef

  -- table to hold buttons
  let rows = 10
  let columns = 5
  buttonBox <- GTK.tableNew rows columns False
  --GTK.widgetShowAll table

  -- close tab button
  cb <- GTK.buttonNewWithLabel "Close"
  GTK.widgetModifyBg cb GTK.StatePrelight (GTK.Color 65535 32767 32767)
  GTK.on cb GTK.buttonActivated $ closePage notebook vbox
  GTK.widgetShowAll cb

  -- start/restart button
  sb <- GTK.buttonNewWithLabel "Start"
  GTK.widgetModifyBg sb GTK.StatePrelight (GTK.Color 32767 65535 32767)
  GTK.on sb GTK.buttonActivated $ press buttonBox socket title refproc dir commandList
  GTK.widgetShowAll sb

  -- button ordering start top, close bottom
  GTK.tableAttachDefaults buttonBox sb 0 columns 0 (rows - 1)
  GTK.tableAttachDefaults buttonBox cb 1 (columns - 1) (rows - 1) rows
  GTK.containerAdd vbox buttonBox

  -- automatically start sub-process?
  if autoStart
    then return ()
    else do
      GTK.widgetShowAll buttonBox

  page <- GTK.notebookAppendPage notebook vbox title
  tabLabel <- GTK.notebookGetTabLabel notebook vbox
  setTabTextColour tabLabel stopped
  -- new page is reordereable
  GTK.notebookSetTabReorderable notebook vbox True

  GTK.on socket GTK.socketPlugRemoved $ unplug sb buttonBox tabLabel stopped socket refproc
  GTK.on socket GTK.socketPlugAdded $ plug tabLabel running socket sendList

  if autoStart
    then do
      runC refproc socket title dir commandList
    else return ()

  return page

-- remove a closed page
closePage :: GTK.Notebook -> GTK.VBox -> IO ()
closePage notebook page = do
  pageNumber <-  GTK.notebookPageNum notebook page
  case pageNumber of
    Nothing ->  return ()
    Just pageIndex ->  do
      GTK.notebookRemovePage notebook pageIndex
      return ()


-- prevent reorder < 1st place
reordered :: GTK.Notebook -> GTK.Widget -> Int -> IO ()
reordered notebook page position = do
  if position < 2
    then GTK.notebookReorderChild notebook page 1
    else return ()


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
press :: GTK.Table -> GTK.Socket -> String -> PR.ProcRef -> Maybe String -> CP.CommandList -> IO ()
press buttons socket title refproc dir commandList = do
  GTK.widgetHide buttons
  runC refproc socket title dir commandList
  GTK.widgetGrabFocus socket


-- detect the program creating its main window
-- delay in order to give it time to set itself up
-- send too quickly and the event queue locks up
plug :: Maybe GTK.Widget -> Maybe GTK.Color -> GTK.Socket -> [String] -> IO ()
plug tabLabel colour socket sendList = do
  h <- GTK.timeoutAdd (delayedSend tabLabel colour socket sendList) 1000
  return ()


-- dummy routine to send a couple of test lines
delayedSend :: Maybe GTK.Widget -> Maybe GTK.Color -> GTK.Socket -> [String] -> IO Bool
delayedSend tabLabel colour socket sendList = do
  mapM_ (SC.sendLine socket) sendList
  setTabTextColour tabLabel colour
  return False


-- dialog to decide whether to restart the command
unplug :: GTK.Button ->  GTK.Table -> Maybe GTK.Widget -> Maybe GTK.Color -> GTK.Socket ->  PR.ProcRef -> IO Bool
unplug startButton otherButtons tabLabel colour socket refproc = do
  GTK.widgetHide socket
  GTK.buttonSetLabel startButton "Restart"

  PR.shutdown refproc

  GTK.widgetShowAll otherButtons

  setTabTextColour tabLabel colour

  return True


-- set the text colour of a tab label
setTabTextColour :: Maybe GTK.Widget -> Maybe GTK.Color -> IO ()
setTabTextColour (Just tabLabel) (Just colour) = do
  GTK.widgetModifyFg tabLabel GTK.StateNormal colour
  GTK.widgetModifyFg tabLabel GTK.StateActive colour
setTabTextColour _ _ = return ()


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
