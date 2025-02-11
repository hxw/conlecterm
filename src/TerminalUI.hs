-- Copyright (c) 2012-2019, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

{-# LANGUAGE OverloadedStrings #-}

module TerminalUI(run
                 ) where

import Data.Foldable( foldlM )
import Data.List( find )
import qualified Data.Text as T
import Control.Monad ( when, void )
import Control.Monad.Trans( liftIO )
import qualified System.Environment as Env

import qualified GI.Gtk as GTK
import qualified GI.Gdk as GDK
import qualified GI.GLib as GLIB
import qualified GI.Gdk.Objects.Screen as Screen
import qualified GI.Gtk.Objects.StyleContext as CTX
import qualified GI.Gtk.Objects.CssProvider as CSS
import Data.GI.Base.ShortPrelude

import qualified SessionParser as SP
import qualified ConfigurationParser as CP
import qualified ProcessRunner as PR
import qualified SendControl as SC

initialTitle :: T.Text
initialTitle = "Conlecterm"

-- list of possible icons from default theme
iconNameList :: [T.Text]
iconNameList = [ "conlecterm"
               , "utilities-terminal"
               , "gnome-terminal"
               , "xfce-terminal"
               , "terminal"
               ]

orientation :: SP.Orientation -> GTK.PositionType
orientation SP.LeftTabs   = GTK.PositionTypeLeft
orientation SP.RightTabs  = GTK.PositionTypeRight
orientation SP.TopTabs    = GTK.PositionTypeTop
orientation SP.BottomTabs = GTK.PositionTypeBottom


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

  -- need prog name and args to init Gtk properly
  -- so that the class name will be set to prog name
  prog <- Env.getProgName
  args <- Env.getArgs
  void . GTK.init . Just . map T.pack $ prog : args

  setupCSS $ T.pack cssFileName

  toplevel <- GTK.windowNew GTK.WindowTypeToplevel
  notebook <- GTK.notebookNew
  GTK.widgetSetName notebook "conlecterm"

  GTK.onNotebookPageReordered notebook $ reordered notebook

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
  GTK.widgetShow toplevel

  -- set up page switcher
  table <- GTK.gridNew
  GTK.setGridColumnHomogeneous table True
  GTK.setGridRowHomogeneous table True

  ctxTable <- GTK.widgetGetStyleContext table
  CTX.styleContextAddClass ctxTable "new_table"

  _ <- GTK.onNotebookSwitchPage notebook $ pageChange toplevel notebook table configFileName cssFileName
  GTK.widgetSetCanFocus notebook False

  -- create the buttons page
  GTK.widgetShow table

  tabLabel <- GTK.labelNew $ Just "+NEW"
  ctxNew <- GTK.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxNew "new_tab"

  buttonPage <- GTK.notebookAppendPage notebook table $ Just tabLabel

  -- create all the initial table entries
  mapM_ (\tab ->  do
            let (title, start, dir, command, sendList, cssClass) = tab
            addPane notebook (T.pack title) start dir command sendList (T.pack cssClass)) tabList

  -- do not close app if there are active tabs
  GTK.onWidgetDeleteEvent toplevel $ \e -> do
    checkExit toplevel sessionName sessionFileName orient notebook
  GTK.onWidgetDestroy toplevel GTK.mainQuit

  GTK.widgetShowAll toplevel

  -- key press signal
  _ <- GTK.onWidgetKeyPressEvent toplevel $ \e -> do
        k <- GDK.getEventKeyKeyval e
        mods <- GDK.getEventKeyState e
        liftIO $ forwardKeyPress notebook mods k
        return True

  -- start the GTK event loop
  GTK.main


-- create a css provider for the buttons
setupCSS :: T.Text -> IO ()
setupCSS cssFileName = do

  css <- CSS.cssProviderNew
  CSS.cssProviderLoadFromPath css cssFileName

  screen <- Screen.screenGetDefault
  case screen of
    Nothing -> return ()
    Just scn -> CTX.styleContextAddProviderForScreen scn css 800


-- send key to the right socket
forwardKeyPress :: GTK.Notebook -> [GDK.ModifierType] -> Word32 -> IO ()
forwardKeyPress notebook mods key = do
  pageNumber <- GTK.notebookGetCurrentPage notebook
  tab <- GTK.notebookGetNthPage notebook pageNumber
  case tab of
    Nothing -> return ()
    Just page -> do
      ctr <- castTo GTK.Container page
      case ctr of
        Nothing -> return ()
        Just ctr -> do
                  n <- GTK.containerGetChildren ctr
                  let first = head n
                  s <- castTo GTK.Socket first
                  case s of
                    Nothing -> return ()
                    Just socket -> do
                           socketId <- GTK.socketGetId socket
                           pw <- GTK.socketGetPlugWindow socket
                           case pw of
                             Nothing -> return ()
                             Just _pw -> SC.sendKey socket mods key

-- compile buttons from current configuration
createButtons :: String -> String -> GTK.Grid -> GTK.Notebook -> IO ()
createButtons configFileName cssFileName table notebook = do

  -- erase old contents
  contents <- GTK.containerGetChildren table
  mapM_ (\item -> GTK.containerRemove table item) contents

  -- reload CSS
  setupCSS $ T.pack cssFileName

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
                            addButton table x y notebook (T.pack title) start dir command sendList (T.pack cssClass)
                            if x > 4 then return (0, y + 1) else return (x + 1, y)
                         ) (0, 0) allTabs
              --GTK.widgetShowAll table
              GTK.widgetShow table
              return ()


-- do not allow exit if still some tabs are open
checkExit :: GTK.Window -> String -> String -> SP.Orientation -> GTK.Notebook -> IO Bool
checkExit window sessionName sessionFileName orient notebook = do
  saveSession
  active <- PR.activeProcs
  if active > 0 then do
                exitNotice active
                return True
  else return False

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
              Just page -> do
                       t <- GTK.widgetGetName page
                       return $ T.unpack t


-- dialog warning about acive tabs
exitNotice :: Int -> IO ()
exitNotice active = do
  -- (Just window) [GTK.DialogFlagsDestroyWithParent] GTK.MessageTypeWarning GTK.ButtonsTypeOk "Some tabs are still active"
  dialog <- GTK.dialogNew
  let verb = if active == 1 then "is" else "are"
  let message = "<span foreground=\"red\" size=\"xx-large\">There " ++ verb ++ " " ++ (show active) ++ " tabs are still active</span>"
  area <- GTK.dialogGetContentArea dialog
  label <- GTK.labelNew $ Just ""
  GTK.labelSetMarkup label $ T.pack message
  GTK.widgetShow label
  GTK.containerAdd area label
  GTK.dialogAddButton dialog "OK" 42
  response <- GTK.dialogRun dialog
  GTK.widgetDestroy dialog
  return ()

-- add buttons to the button menu
addButton :: GTK.Grid -> Int32 -> Int32 -> GTK.Notebook -> T.Text -> Bool -> Maybe String -> CP.CommandList -> [String] -> T.Text -> IO ()
addButton table x y notebook title autoStart dir commandList sendList cssClass = do
  label <- GTK.labelNew $ Just title
  ctx1 <- GTK.widgetGetStyleContext label
  CTX.styleContextAddClass ctx1 "item_button"
  CTX.styleContextAddClass ctx1 cssClass

  button <- GTK.buttonNew

  GTK.widgetSetName button title
  GTK.containerAdd button label

  _ <- GTK.onButtonClicked button $ (addPane notebook title autoStart dir commandList sendList cssClass >> return ())

  GTK.widgetShow button
  GTK.gridAttach table button x y 1 1
  return ()


-- add auto/manual started panes
addPane :: GTK.Notebook ->  T.Text -> Bool -> Maybe String -> CP.CommandList -> [String] -> T.Text -> IO Int32
addPane notebook title autoStart dir commandList sendList cssClass = do

  vbox <- GTK.boxNew GTK.OrientationVertical 0 -- 0 pixel spacing
  GTK.boxSetHomogeneous vbox False
  GTK.widgetSetCanFocus vbox False
  GTK.widgetSetName vbox title
  GTK.widgetSetVexpand vbox True
  GTK.widgetShow vbox

  -- create a socket and put it in the Vbox
  socket <- GTK.socketNew
  GTK.widgetSetCanFocus socket True
  GTK.containerAdd vbox socket

  -- to hold the process that will be started later
  refproc <- PR.newProcRef

  -- table to hold buttons
  let rows = 10
  let columns = 5
  let startHeight = rows - 1
  let startWidth = columns
  let startX = 0
  let startY = 0
  let removeHeight = rows - startHeight
  let removeWidth = columns - 2
  let removeX = 1
  let removeY = startHeight

  buttonBox <- GTK.gridNew
  GTK.widgetSetVexpand buttonBox True

  GTK.setGridColumnHomogeneous buttonBox True
  GTK.setGridRowHomogeneous buttonBox True

  -- remove tab button
  rtLabel <- GTK.labelNew $ Just "Remove Tab"
  ctxRtLabel <- GTK.widgetGetStyleContext rtLabel
  CTX.styleContextAddClass ctxRtLabel "remove_tab_button"

  rtBtn <- GTK.buttonNew
  ctxRtBtn <- GTK.widgetGetStyleContext rtBtn
  CTX.styleContextAddClass ctxRtBtn "remove_tab_button"

  GTK.containerAdd rtBtn rtLabel

  _ <- GTK.onButtonClicked rtBtn $ closePage notebook vbox
  GTK.widgetShow rtBtn

  -- start button
  stLabel <- GTK.labelNew $ Just "Start"
  ctxStLabel <- GTK.widgetGetStyleContext stLabel
  CTX.styleContextAddClass ctxStLabel "start_button"

  stBtn <- GTK.buttonNew
  ctxStBtn <- GTK.widgetGetStyleContext stBtn
  CTX.styleContextAddClass ctxStBtn "start_button"

  GTK.containerAdd stBtn stLabel

  _ <- GTK.onButtonClicked stBtn $ press buttonBox socket title refproc dir commandList
  GTK.widgetShow stBtn

  -- button ordering start top, close bottom
  GTK.gridAttach buttonBox stBtn startX startY startWidth startHeight
  GTK.gridAttach buttonBox rtBtn removeX removeY removeWidth removeHeight

  GTK.containerAdd vbox buttonBox

  -- automatically start sub-process?
  when (not autoStart) $ GTK.widgetShow buttonBox

  tabLabel <- GTK.labelNew $ Just title
  ctxTL <- GTK.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxTL "item_tab"
  CTX.styleContextAddClass ctxTL cssClass
  page <- GTK.notebookAppendPage notebook vbox $ Just tabLabel

  setTabStopped tabLabel

  -- new page is can be reordered
  GTK.notebookSetTabReorderable notebook vbox True

  _ <- GTK.onSocketPlugRemoved socket $ unplug buttonBox tabLabel socket refproc
  _ <- GTK.onSocketPlugAdded socket $ plug tabLabel socket sendList

  when autoStart $ runC refproc socket title dir commandList

  return page

-- remove a closed page
closePage :: GTK.Notebook -> GTK.Box -> IO ()
closePage notebook page = do
  pageNumber <- GTK.notebookPageNum notebook page
  GTK.notebookRemovePage notebook pageNumber


-- prevent reorder < 1st place
reordered :: GTK.Notebook -> GTK.Widget -> Word32 -> IO ()
reordered notebook page position = do
  when (position < 2) $ GTK.notebookReorderChild notebook page 1


-- run a command
runC :: PR.ProcRef -> GTK.Socket -> T.Text -> Maybe String -> CP.CommandList -> IO ()
runC refproc socket title dir commandList = do
  --GTK.widgetShowAll socket
  GTK.widgetSetVexpand socket True
  GTK.widgetShow socket

  -- expand the command string
  paneid <- GTK.socketGetId socket
  let windowID = toInteger paneid :: Integer
  let cmd = CP.expandCommand commandList windowID (T.unpack title)
  PR.run refproc dir cmd


-- button pressed
press :: GTK.Grid -> GTK.Socket -> T.Text -> PR.ProcRef -> Maybe String -> CP.CommandList -> IO ()
press buttons socket title refproc dir commandList = do
  GTK.widgetHide buttons
  runC refproc socket title dir commandList
  GTK.widgetGrabFocus socket


-- detect the program creating its main window
-- delay in order to give it time to set itself up
-- send too quickly and the event queue locks up
plug :: GTK.Label -> GTK.Socket -> [String] -> IO ()
plug tabLabel socket sendList = do
  _h <- GLIB.timeoutAdd GLIB.PRIORITY_HIGH 1000 (delayedSend tabLabel socket sendList)
  return ()


-- routine to send the text lines
delayedSend :: GTK.Label -> GTK.Socket -> [String] -> IO Bool
delayedSend tabLabel socket sendList = do
  mapM_ (SC.sendLine socket) sendList
  setTabRunning tabLabel
  return False


-- dialog to decide whether to restart the command
unplug :: GTK.Grid -> GTK.Label -> GTK.Socket ->  PR.ProcRef -> IO Bool
unplug otherButtons tabLabel socket refproc = do
  GTK.widgetHide socket

  PR.shutdown refproc

  --GTK.widgetShowAll otherButtons
  GTK.widgetShow otherButtons

  setTabStopped tabLabel

  return True


-- so CSS can set the text colour of a tab label
setTabRunning :: GTK.Label -> IO ()
setTabRunning tabLabel = do
  ctxTab <- GTK.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxTab "running"

setTabStopped :: GTK.Label -> IO ()
setTabStopped tabLabel = do
  ctxTab <- GTK.widgetGetStyleContext tabLabel
  CTX.styleContextRemoveClass ctxTab "running"


-- change the main title to be the tab name
pageChange :: GTK.Window -> GTK.Notebook -> GTK.Grid -> String -> String -> GTK.Widget -> Word32 -> IO ()
pageChange window notebook table configFileName cssFileName w pageW = do
  let page = fromInteger (toInteger pageW) :: Int32
  when (page == 0) $ createButtons configFileName cssFileName table notebook
  changeTitle window notebook page


changeTitle :: GTK.Window -> GTK.Notebook -> Int32 -> IO ()
changeTitle window notebook page = do
  vBox <- GTK.notebookGetNthPage notebook page
  case vBox of
    Nothing ->  GTK.set window [GTK.windowTitle GTK.:= initialTitle]

    Just thePage -> do
              text <- GTK.notebookGetTabLabelText notebook thePage
              let sep = " - " :: T.Text
              let title = case text of
                            Nothing -> initialTitle <> sep <> (T.pack $ show page)
                            Just s  -> initialTitle <> sep <> s
              GTK.setWindowTitle window title
