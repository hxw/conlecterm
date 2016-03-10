-- Copyright (c) 2012-2015, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module TerminalUI(run
                 ) where

import Data.Foldable( foldlM )
import Data.List( find )
import Control.Monad (when)
import Control.Monad.Trans( liftIO )
--import System.Process( ProcessHandle )
--import System.IO

import qualified Graphics.UI.Gtk as GTK
import Graphics.Rendering.Pango as Pango

import qualified SessionParser as SP
import qualified ConfigurationParser as CP
import qualified ProcessRunner as PR
import qualified SendControl as SC

initialTitle :: String
initialTitle = "Conlecterm"

-- list of possible icons from default theme
iconNameList :: [String]
iconNameList = ["utilities-terminal", "gnome-terminal", "xfce-terminal", "terminal"]

orientation :: SP.Orientation -> GTK.PositionType
orientation SP.LeftTabs   = GTK.PosLeft
orientation SP.RightTabs  = GTK.PosRight
orientation SP.TopTabs    = GTK.PosTop
orientation SP.BottomTabs = GTK.PosBottom


-- text sizes and weights
tabTextSize :: (Double, Weight)
tabTextSize = (11, Pango.WeightBold)

buttonTextSize :: (Double, Weight)
buttonTextSize = (16, Pango.WeightMedium)

startTextSize :: (Double, Weight)
startTextSize = (64, Pango.WeightBold)

closeTextSize :: (Double, Weight)
closeTextSize = (32, Pango.WeightBold)


-- colours for buttons
type Colours = (GTK.Color, GTK.Color, GTK.Color)

startButtonColours :: Colours
startButtonColours = (startNormalColour, startHoverColour, startActiveColour)
    where
      startNormalColour = GTK.Color 10000 20000 10000
      startHoverColour  = GTK.Color 16383 32767 16383
      startActiveColour = GTK.Color 32767 65535 32767

closeButtonColours :: Colours
closeButtonColours = (closeNormalColour, closeHoverColour, closeActivecolour)
    where
      closeNormalColour = GTK.Color 20000 10000 10000
      closeHoverColour  = GTK.Color 32767 16383 16383
      closeActivecolour = GTK.Color 65535 32767 32767


-- compile the configuration
compileConfigs :: String
                        -> String
                        -> IO
                             (Maybe
                                (String,
                                 SP.Orientation,
                                 [CP.PaneInfo],
                                 CP.Hashes))
compileConfigs configFileName sessionFileName = do
  mh <- CP.compile [configFileName]
  case mh of
    Nothing -> return Nothing
    Just h -> do
      session <- SP.readSession sessionFileName
      case session of
        Nothing -> return Nothing
        Just s -> do
               let SP.Session name orient tabs = s
               tabList <- CP.expandPanes h tabs
               return $ Just (name, orient, tabList, h)


run :: String -> String -> IO (Maybe String)
run configFileName sessionFileName = do

  r <- compileConfigs configFileName sessionFileName
  case r of
    Nothing -> return $ Just "error in configuration file"
    Just session -> do
           run' configFileName sessionFileName session
           return Nothing

run' :: String -> String -> (String, SP.Orientation, [CP.PaneInfo], CP.Hashes) -> IO ()
run' configFileName sessionFileName (sessionName, orient, tabList, _h) = do

  _ <- GTK.initGUI

  toplevel <- GTK.windowNew
  notebook <- GTK.notebookNew

  _ <- GTK.on notebook GTK.pageReordered $ reordered notebook

  GTK.notebookSetTabPos notebook $ orientation orient
  GTK.notebookSetHomogeneousTabs notebook True

  GTK.windowSetDefaultSize toplevel 800 600
  GTK.set toplevel [GTK.windowTitle GTK.:= initialTitle]

  -- get the an icon from the default theme
  theme <- GTK.iconThemeGetDefault

  -- check if selection is available
  availableIcons <-mapM (\iconName ->  do
             exists <- GTK.iconThemeHasIcon theme iconName
             return (iconName, exists)) iconNameList
  -- putStrLn $ "icons = " ++ (show availableIcons)

  -- if an icon is found assign it to toplevel window
  let theIcon = find (\ (_name, exists) -> exists) availableIcons
  -- putStrLn $ "icon = " ++ (show theIcon)
  case theIcon of
    Nothing -> return ()
    Just (iconName, _) ->  do
      GTK.windowSetDefaultIconName iconName
      GTK.windowSetIconName toplevel iconName

  toplevel `GTK.containerAdd` notebook
  GTK.widgetShowAll toplevel

  -- set up page switcher
  table <- GTK.tableNew 4 4 True
  _ <- GTK.on notebook GTK.switchPage $ pageChange toplevel notebook table configFileName
  GTK.widgetSetCanFocus notebook False

  -- create the buttons page
  GTK.widgetShowAll table
  _buttonPage <- GTK.notebookAppendPage notebook table "+NEW"
  tabLabel <- GTK.notebookGetTabLabel notebook table
  setTabTextColour tabLabel $ Just (GTK.Color 32767 0 32767)
  setTabTextSize tabLabel tabTextSize

  -- create all the initial table
  mapM_ (\tab ->  do
            let (title, start, dir, command, sendList, running, stopped) = tab
            addPane notebook title start dir command sendList running stopped) tabList

  -- link up the close button
  _ <- GTK.on toplevel GTK.deleteEvent $ liftIO $ checkExit sessionName sessionFileName orient notebook

  -- start the GTK event loop
  GTK.mainGUI


-- compile buttons from current configuration

createButtons :: String -> GTK.Table -> GTK.Notebook -> IO ()
createButtons configFileName table notebook = do

  -- erase old contents
  contents <- GTK.containerGetChildren table
  mapM_ (\item -> GTK.containerRemove table item) contents

  -- fetch possible updated configuration
  mh <- CP.compile [configFileName]
  case mh of
    Nothing -> do
            putStrLn $ "error re-reading configuration"
            return ()
    Just cfg -> create' cfg

      where create' h = do
              allTabs <- CP.sortedTabs h
              _ <- foldlM (\(x, y) item ->  do
                            let (title, start, dir, command, sendList, running, stopped) = item
                            addButton table x y notebook title start dir command sendList running stopped
                            let x1 = x + 1
                            if x > 4 then return (0, y + 1) else return (x1, y)
                         ) (0, 0) allTabs
              GTK.widgetShowAll table
              return ()


-- do not allow exit if still some tabs are open
checkExit :: String ->  String -> SP.Orientation -> GTK.Notebook -> IO Bool
checkExit sessionName sessionFileName orient notebook = do
  saveSession
  active <- PR.activeProcs
  let continue = active > 0
  if continue then exitNotice else GTK.mainQuit
  return $ continue

  where

    saveSession :: IO ()
    saveSession = do
      pageCount <- GTK.notebookGetNPages notebook
      tabs <- mapM oneTab [1 .. pageCount - 1]
      let s = SP.Session sessionName orient tabs
      SP.writeSession s sessionFileName
        where
          oneTab i = do
            p <- GTK.notebookGetNthPage notebook i
            case p of
              Nothing -> return ""
              Just page -> GTK.widgetGetName page


-- dialog warning about acive tabs
exitNotice :: IO ()
exitNotice = do
  dialog <- GTK.messageDialogNew Nothing [GTK.DialogDestroyWithParent] GTK.MessageWarning GTK.ButtonsOk "Some tabs are still active"
  _response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog


-- add buttons to the button menu
addButton :: GTK.Table -> Int -> Int -> GTK.Notebook -> String -> Bool -> Maybe String -> CP.CommandList -> [String] -> Maybe GTK.Color -> Maybe GTK.Color -> IO ()
addButton table x y notebook title autoStart dir commandList sendList running stopped = do
  label <- GTK.labelNew $ Just title
  setLabelTextSize label buttonTextSize
  button <- GTK.buttonNew

  GTK.widgetSetName button title
  GTK.containerAdd button label


  -- let r = case running of
  --           Nothing -> GTK.Color 32767 32767 32767
  --           Just colour -> colour
  -- let s = case stopped of
  --           Nothing -> GTK.Color 8191 9191 32767
  --           Just colour -> colour

  -- putStrLn $ "r = " ++ (show r)
  -- putStrLn $ "s = " ++ (show s)

  -- c <- GTK.containerGetChildren button
  -- case c of
  --   [] -> return ()
  --   label2:_ -> do
  --          GTK.widgetModifyFg label2 GTK.StateNormal r
  --          GTK.widgetModifyFg label2 GTK.StatePrelight r
  --          GTK.widgetModifyFg label2 GTK.StateSelected r
  --          GTK.widgetModifyFg label2 GTK.StateActive r

  -- GTK.widgetModifyBg label GTK.StateNormal (GTK.Color 65000 65000 65000)
  -- GTK.widgetModifyFg label GTK.StateNormal (GTK.Color 65000 65000 65000)
  -- GTK.widgetModifyText label GTK.StateNormal (GTK.Color 65000 65000 65000)
  -- GTK.widgetModifyFg button GTK.StateNormal (GTK.Color 65000 65000 65000)
  -- GTK.widgetModifyText button GTK.StateNormal (GTK.Color 65000 65000 65000)


  -- GTK.widgetModifyBg button GTK.StateNormal r
  -- GTK.widgetModifyBg button GTK.StatePrelight s
  -- GTK.widgetModifyBg button GTK.StateSelected s
  --GTK.widgetModifyBg button GTK.StateActive r

  --GTK.widgetModifyBg button GTK.StateNormal (GTK.Color 32000 32000 40000)
  --GTK.widgetModifyBg button GTK.StatePrelight (GTK.Color 8191 8191 20000)
  --GTK.widgetModifyBg button GTK.StateSelected (GTK.Color 8191 8191 20000)
  -- GTK.widgetModifyBg button GTK.StateActive (GTK.Color 10000 58000 60000)

  _ <- GTK.on button GTK.buttonActivated $ (addPane notebook title autoStart dir commandList sendList running stopped >> return ())

  GTK.widgetShowAll button
  GTK.tableAttachDefaults table button x (x + 1) y (y + 1)
  return ()


-- add auto/manual started panes
addPane :: GTK.Notebook ->  String -> Bool -> Maybe String -> CP.CommandList -> [String] -> Maybe GTK.Color -> Maybe GTK.Color -> IO Int
addPane notebook title autoStart dir commandList sendList running stopped = do
  vbox <- GTK.vBoxNew False 0
  GTK.widgetSetCanFocus vbox False
  GTK.widgetSetName vbox title

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

  -- -- close tab button
  -- cb <- GTK.buttonNewWithLabel "Close"
  -- GTK.widgetModifyBg cb GTK.StatePrelight (GTK.Color 65535 32767 32767)
  -- _ <- GTK.on cb GTK.buttonActivated $ closePage notebook vbox
  -- GTK.widgetShowAll cb

  -- close tab button
  cl <- GTK.labelNew $ Just "Close"
  setLabelTextSize cl closeTextSize
  cb <- GTK.buttonNew
  setButtonColours cb closeButtonColours
  GTK.containerAdd cb cl

  _ <- GTK.on cb GTK.buttonActivated $ closePage notebook vbox
  GTK.widgetShowAll cb

  -- start/restart button
  sl <- GTK.labelNew $ Just "Start"
  setLabelTextSize sl startTextSize
  sb <- GTK.buttonNew
  setButtonColours sb startButtonColours
  GTK.containerAdd sb sl

  _ <- GTK.on sb GTK.buttonActivated $ press buttonBox socket title refproc dir commandList
  GTK.widgetShowAll sb

  -- button ordering start top, close bottom
  GTK.tableAttachDefaults buttonBox sb 0 columns 0 (rows - 1)
  GTK.tableAttachDefaults buttonBox cb 1 (columns - 1) (rows - 1) rows
  GTK.containerAdd vbox buttonBox

  -- automatically start sub-process?
  when (not autoStart) $ GTK.widgetShowAll buttonBox

  page <- GTK.notebookAppendPage notebook vbox title
  tabLabel <- GTK.notebookGetTabLabel notebook vbox
  setTabTextColour tabLabel stopped
  setTabTextSize tabLabel tabTextSize

  -- new page is reordereable
  GTK.notebookSetTabReorderable notebook vbox True

  _ <- GTK.on socket GTK.socketPlugRemoved $ unplug buttonBox tabLabel stopped socket refproc
  _ <- GTK.on socket GTK.socketPlugAdded $ plug tabLabel running socket sendList

  when autoStart $ runC refproc socket title dir commandList

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
  when (position < 2) $ GTK.notebookReorderChild notebook page 1


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
  _h <- GTK.timeoutAdd (delayedSend tabLabel colour socket sendList) 1000
  return ()


-- dummy routine to send a couple of test lines
delayedSend :: Maybe GTK.Widget -> Maybe GTK.Color -> GTK.Socket -> [String] -> IO Bool
delayedSend tabLabel colour socket sendList = do
  mapM_ (SC.sendLine socket) sendList
  setTabTextColour tabLabel colour
  return False


-- dialog to decide whether to restart the command
unplug :: GTK.Table -> Maybe GTK.Widget -> Maybe GTK.Color -> GTK.Socket ->  PR.ProcRef -> IO Bool
unplug otherButtons tabLabel colour socket refproc = do
  GTK.widgetHide socket

  PR.shutdown refproc

  GTK.widgetShowAll otherButtons

  setTabTextColour tabLabel colour

  return True


-- set the colours of a button
setButtonColours :: GTK.Button -> Colours -> IO ()
setButtonColours button (normal, hover, active) = do
  GTK.widgetModifyBg button GTK.StateNormal normal
  GTK.widgetModifyBg button GTK.StatePrelight hover
  GTK.widgetModifyBg button GTK.StateSelected hover
  GTK.widgetModifyBg button GTK.StateActive active


-- set the text colour of a tab label
setTabTextColour :: Maybe GTK.Widget -> Maybe GTK.Color -> IO ()
setTabTextColour (Just tabLabel) (Just colour) = do
  GTK.widgetModifyFg tabLabel GTK.StateNormal colour
  GTK.widgetModifyFg tabLabel GTK.StateActive colour
setTabTextColour _ _ = return ()


-- set font size/weight of a tab label
setTabTextSize :: Maybe GTK.Widget -> (Double, Weight) -> IO ()
setTabTextSize (Just tabLabel) (fontSize, fontWeight) = do
  let label = GTK.castToLabel tabLabel
  setLabelTextSize label (fontSize, fontWeight)
setTabTextSize _ _ = return ()

setLabelTextSize :: GTK.Label -> (Double, Weight) -> IO ()
setLabelTextSize label (fontSize, fontWeight) = do
  --GTK.labelSetAttributes label [Pango.AttrWeight 0 999 fontWeight, Pango.AttrSize 0 999 fontSize, Pango.AttrForeground 0 999 (GTK.Color 0 0 0)]
  GTK.labelSetAttributes label [Pango.AttrWeight 0 999 fontWeight, Pango.AttrSize 0 999 fontSize]


-- change the main title to be the tab name
pageChange :: GTK.Window -> GTK.Notebook -> GTK.Table -> String -> Int -> IO ()
pageChange window notebook table configFileName page = do
  when (page == 0) $ createButtons configFileName table notebook
  changeTitle window notebook page


changeTitle :: GTK.Window -> GTK.Notebook -> Int -> IO ()
changeTitle window notebook page = do
  vBox <- GTK.notebookGetNthPage notebook page
  case vBox of
    Nothing ->  GTK.set window [GTK.windowTitle GTK.:= initialTitle]

    Just thePage -> do
              text <- GTK.notebookGetTabLabelText notebook thePage
              let title = case text of
                            Nothing -> initialTitle ++ " - " ++ (show page)
                            Just s  -> initialTitle ++ " - " ++ s
              GTK.set window [GTK.windowTitle GTK.:= title]
