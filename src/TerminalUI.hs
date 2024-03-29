-- Copyright (c) 2012-2019, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

module TerminalUI(run
                 ) where

import Data.Foldable( foldlM )
import Data.List( find )
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.Trans( liftIO )

import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.UI.Gtk.Gdk.Screen as Screen
import qualified Graphics.UI.Gtk.General.StyleContext as CTX
import qualified Graphics.UI.Gtk.General.CssProvider as CSS

import qualified SessionParser as SP
import qualified ConfigurationParser as CP
import qualified ProcessRunner as PR
import qualified SendControl as SC

initialTitle :: String
initialTitle = "Conlecterm"

-- list of possible icons from default theme
iconNameList :: [String]
iconNameList = [ "conlecterm"
               , "utilities-terminal"
               , "gnome-terminal"
               , "xfce-terminal"
              , "terminal"
               ]

orientation :: SP.Orientation -> GTK.PositionType
orientation SP.LeftTabs   = GTK.PosLeft
orientation SP.RightTabs  = GTK.PosRight
orientation SP.TopTabs    = GTK.PosTop
orientation SP.BottomTabs = GTK.PosBottom


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


run :: String -> String -> String -> Bool -> IO (Maybe String)
run configFileName cssFileName sessionFileName verbose = do

  r <- compileConfigs configFileName sessionFileName
  case r of
    Nothing -> return $ Just "error in configuration file"
    Just session -> do
           run' configFileName cssFileName sessionFileName session verbose
           return Nothing

run' :: String -> String -> String -> (String, SP.Orientation, [CP.PaneInfo], CP.Hashes) -> Bool -> IO ()
run' configFileName cssFileName sessionFileName (sessionName, orient, tabList, _h) verbose = do


  _ <- GTK.initGUI

  setupCSS cssFileName

  toplevel <- GTK.windowNew
  notebook <- GTK.notebookNew
  GTK.widgetSetName notebook "conlecterm"

  _ <- GTK.on notebook GTK.pageReordered $ reordered notebook

  GTK.notebookSetTabPos notebook $ orientation orient
--  GTK.set notebook [GTK.notebookHomogeneous GTK.:= True]
--  GTK.set notebook [GTK.notebookTabVborder GTK.:= 0]
  GTK.set notebook [GTK.notebookScrollable GTK.:= True]

  GTK.windowSetDefaultSize toplevel 800 600
  GTK.windowMaximize toplevel
  GTK.set toplevel [GTK.windowTitle GTK.:= initialTitle]

  -- get the an icon from the default theme
  theme <- GTK.iconThemeGetDefault

  -- check if selection is available
  availableIcons <- mapM (\iconName ->  do
             exists <- GTK.iconThemeHasIcon theme iconName
             return (iconName, exists)) iconNameList

  -- if an icon is found assign it to toplevel window
  let theIcon = find (\ (_name, exists) -> exists) availableIcons

  if verbose
    then do
      putStrLn $ "available icons: " ++ (show availableIcons)
      putStrLn $ "selected icon:   " ++ (show theIcon)
    else
      return ()

  case theIcon of
    Nothing -> return ()
    Just (iconName, _) ->  do
      GTK.windowSetDefaultIconName iconName
      GTK.set toplevel [GTK.windowIconName GTK.:= iconName]

  toplevel `GTK.containerAdd` notebook
  GTK.widgetShowAll toplevel

  -- set up page switcher
  table <- GTK.tableNew 4 4 True
  ctxTable <- GTK.widgetGetStyleContext table
  CTX.styleContextAddClass ctxTable "new_table"

  _ <- GTK.on notebook GTK.switchPage $ pageChange toplevel notebook table configFileName cssFileName
  GTK.widgetSetCanFocus notebook False

  -- create the buttons page
  GTK.widgetShowAll table
  _buttonPage <- GTK.notebookAppendPage notebook table "+NEW"
  tabLabel <- GTK.notebookGetTabLabel notebook table

  case tabLabel of
    Nothing -> return ()
    Just tl -> do
      ctxNew <- GTK.widgetGetStyleContext tl
      CTX.styleContextAddClass ctxNew "new_tab"

  -- create all the initial table
  mapM_ (\tab ->  do
            let (title, start, dir, command, sendList, cssClass) = tab
            addPane notebook title start dir command sendList cssClass) tabList

  -- link up the remove tab button
  _ <- GTK.on toplevel GTK.deleteEvent $ liftIO $ checkExit toplevel sessionName sessionFileName orient notebook

  -- key press signal
  _ <- GTK.on toplevel GTK.keyPressEvent $ do
        k <- GTK.eventKeyVal
        name <- GTK.eventKeyName
        mods <- GTK.eventModifier
        let mods' = if name == (T.pack "less")
                    then filter (\m -> m /= GTK.Shift) mods
                    else mods
        liftIO $ forwardKeyPress notebook mods' k
        return True

  -- start the GTK event loop
  GTK.mainGUI

-- create a css provider for the buttons
setupCSS :: String -> IO ()
setupCSS cssFileName = do

  css <- CSS.cssProviderNew
  CSS.cssProviderLoadFromPath css cssFileName

  screen <- Screen.screenGetDefault
  case screen of
    Nothing -> return ()
    Just scn -> CTX.styleContextAddProviderForScreen scn css 800

-- send key to the right socket
forwardKeyPress :: GTK.Notebook -> [GTK.Modifier] -> GTK.KeyVal -> IO ()
forwardKeyPress notebook mods key = do
  page <- GTK.notebookGetCurrentPage notebook
  tab <- GTK.notebookGetNthPage notebook page
  case tab of
    Nothing -> return ()
    Just page -> do
             let b = GTK.isA page GTK.gTypeContainer
             when b $ do
                  let c = GTK.castToContainer page
                  n <- GTK.containerGetChildren c
                  let first = head n
                  let b = GTK.isA first GTK.gTypeSocket
                  when b $ do
                        let socket = GTK.castToSocket first
                        b <- GTK.socketHasPlug socket
                        when b $ SC.sendKey socket mods key


-- compile buttons from current configuration
createButtons :: String -> String -> GTK.Table -> GTK.Notebook -> IO ()
createButtons configFileName cssFileName table notebook = do

  -- erase old contents
  contents <- GTK.containerGetChildren table
  mapM_ (\item -> GTK.containerRemove table item) contents

  -- reload CSS
  setupCSS cssFileName

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
                            let (title, start, dir, command, sendList, cssClass) = item
                            addButton table x y notebook title start dir command sendList cssClass
                            let x1 = x + 1
                            if x > 4 then return (0, y + 1) else return (x1, y)
                         ) (0, 0) allTabs
              GTK.widgetShowAll table
              return ()


-- do not allow exit if still some tabs are open
checkExit :: GTK.Window -> String ->  String -> SP.Orientation -> GTK.Notebook -> IO Bool
checkExit window sessionName sessionFileName orient notebook = do
  saveSession
  active <- PR.activeProcs
  let continue = active > 0
  if continue then exitNotice window else GTK.mainQuit
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
exitNotice :: GTK.Window -> IO ()
exitNotice window = do
  dialog <- GTK.messageDialogNew (Just window) [GTK.DialogDestroyWithParent] GTK.MessageWarning GTK.ButtonsOk "Some tabs are still active"
  _response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog


-- add buttons to the button menu
addButton :: GTK.Table -> Int -> Int -> GTK.Notebook -> String -> Bool -> Maybe String -> CP.CommandList -> [String] -> String -> IO ()
addButton table x y notebook title autoStart dir commandList sendList cssClass = do
  label <- GTK.labelNew $ Just title
  ctx1 <- GTK.widgetGetStyleContext label
  CTX.styleContextAddClass ctx1 "item_button"
  CTX.styleContextAddClass ctx1 cssClass

  button <- GTK.buttonNew

  GTK.widgetSetName button title
  GTK.containerAdd button label

  _ <- GTK.on button GTK.buttonActivated $ (addPane notebook title autoStart dir commandList sendList cssClass >> return ())

  GTK.widgetShowAll button
  GTK.tableAttachDefaults table button x (x + 1) y (y + 1)
  return ()


-- add auto/manual started panes
addPane :: GTK.Notebook ->  String -> Bool -> Maybe String -> CP.CommandList -> [String] -> String -> IO Int
addPane notebook title autoStart dir commandList sendList cssClass = do
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

  -- remove tab button
  rtLabel <- GTK.labelNew $ Just "Remove Tab"
  ctxRtLabel <- GTK.widgetGetStyleContext rtLabel
  CTX.styleContextAddClass ctxRtLabel "remove_tab_button"

  rtBtn <- GTK.buttonNew
  ctxRtBtn <- GTK.widgetGetStyleContext rtBtn
  CTX.styleContextAddClass ctxRtBtn "remove_tab_button"

  GTK.containerAdd rtBtn rtLabel

  _ <- GTK.on rtBtn GTK.buttonActivated $ closePage notebook vbox
  GTK.widgetShowAll rtBtn

  -- start button
  stLabel <- GTK.labelNew $ Just "Start"
  ctxStLabel <- GTK.widgetGetStyleContext stLabel
  CTX.styleContextAddClass ctxStLabel "start_button"

  stBtn <- GTK.buttonNew
  ctxStBtn <- GTK.widgetGetStyleContext stBtn
  CTX.styleContextAddClass ctxStBtn "start_button"

  GTK.containerAdd stBtn stLabel

  _ <- GTK.on stBtn GTK.buttonActivated $ press buttonBox socket title refproc dir commandList
  GTK.widgetShowAll stBtn

  -- button ordering start top, close bottom
  GTK.tableAttachDefaults buttonBox stBtn 0 columns 0 (rows - 1)
  GTK.tableAttachDefaults buttonBox rtBtn 1 (columns - 1) (rows - 1) rows
  GTK.containerAdd vbox buttonBox

  -- automatically start sub-process?
  when (not autoStart) $ GTK.widgetShowAll buttonBox

  page <- GTK.notebookAppendPage notebook vbox title
  tabLabel <- GTK.notebookGetTabLabel notebook vbox

  case tabLabel of
    Nothing -> return ()
    Just tl -> do
      ctxTL <- GTK.widgetGetStyleContext tl
      CTX.styleContextAddClass ctxTL "item_tab"
      CTX.styleContextAddClass ctxTL cssClass
  setTabStopped tabLabel

  -- new page is reordereable
  GTK.notebookSetTabReorderable notebook vbox True

  _ <- GTK.on socket GTK.socketPlugRemoved $ unplug buttonBox tabLabel socket refproc
  _ <- GTK.on socket GTK.socketPlugAdded $ plug tabLabel socket sendList

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
plug :: Maybe GTK.Widget -> GTK.Socket -> [String] -> IO ()
plug tabLabel socket sendList = do
  _h <- GTK.timeoutAdd (delayedSend tabLabel socket sendList) 1000
  return ()


-- routine to send the text lines
delayedSend :: Maybe GTK.Widget -> GTK.Socket -> [String] -> IO Bool
delayedSend tabLabel socket sendList = do
  mapM_ (SC.sendLine socket) sendList
  setTabRunning tabLabel
  return False


-- dialog to decide whether to restart the command
unplug :: GTK.Table -> Maybe GTK.Widget -> GTK.Socket ->  PR.ProcRef -> IO Bool
unplug otherButtons tabLabel socket refproc = do
  GTK.widgetHide socket

  PR.shutdown refproc

  GTK.widgetShowAll otherButtons

  setTabStopped tabLabel

  return True


-- so CSS can set the text colour of a tab label
setTabRunning :: Maybe GTK.Widget -> IO ()
setTabRunning (Just tabLabel) = do
  ctxTab <- GTK.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxTab "running"
setTabRunning _ = return ()

setTabStopped :: Maybe GTK.Widget -> IO ()
setTabStopped (Just tabLabel) = do
  ctxTab <- GTK.widgetGetStyleContext tabLabel
  CTX.styleContextRemoveClass ctxTab "running"
setTabStopped _ = return ()


-- change the main title to be the tab name
pageChange :: GTK.Window -> GTK.Notebook -> GTK.Table -> String -> String -> Int -> IO ()
pageChange window notebook table configFileName cssFileName page = do
  when (page == 0) $ createButtons configFileName cssFileName table notebook
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
