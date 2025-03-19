-- Copyright (c) 2012-2025, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TerminalUI(run
                 ) where

import Data.Foldable( foldlM )
import Data.List( find )
import qualified Data.Text as T
import Control.Monad ( when, void )
import Control.Monad.Trans( liftIO )
import qualified System.Environment as Env

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
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

orientation :: SP.Orientation -> Gtk.PositionType
orientation SP.LeftTabs   = Gtk.PositionTypeLeft
orientation SP.RightTabs  = Gtk.PositionTypeRight
orientation SP.TopTabs    = Gtk.PositionTypeTop
orientation SP.BottomTabs = Gtk.PositionTypeBottom


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
  void . Gtk.init . Just . map T.pack $ prog : args

  setupCSS $ T.pack cssFileName

  toplevel <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.widgetSetCanFocus toplevel False

  notebook <- Gtk.notebookNew
  Gtk.widgetSetName notebook "conlecterm"

  Gtk.onNotebookPageReordered notebook $ reordered notebook

  Gtk.notebookSetTabPos notebook $ orientation orient
--  Gtk.set notebook [Gtk.notebookHomogeneous Gtk.:= True]
--  Gtk.set notebook [Gtk.notebookTabVborder Gtk.:= 0]
  Gtk.set notebook [Gtk.notebookScrollable Gtk.:= True]

  Gtk.windowSetDefaultSize toplevel 800 600
  Gtk.windowMaximize toplevel
  Gtk.set toplevel [Gtk.windowTitle Gtk.:= initialTitle]

  -- get the an icon from the default theme
  theme <- Gtk.iconThemeGetDefault

  -- check if selection is available
  availableIcons <- mapM (\iconName ->  do
             exists <- Gtk.iconThemeHasIcon theme iconName
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
      Gtk.windowSetDefaultIconName iconName
      Gtk.set toplevel [Gtk.windowIconName Gtk.:= iconName]

  Gtk.containerAdd toplevel notebook
  Gtk.widgetShow toplevel

  -- set up page switcher
  table <- Gtk.gridNew
  Gtk.setGridColumnHomogeneous table True
  Gtk.setGridRowHomogeneous table True

  ctxTable <- Gtk.widgetGetStyleContext table
  CTX.styleContextAddClass ctxTable "new_table"

  _ <- Gtk.onNotebookSwitchPage notebook $ pageChange toplevel notebook table configFileName cssFileName
  Gtk.widgetSetCanFocus notebook False

  -- create the buttons page
  Gtk.widgetShow table

  tabLabel <- Gtk.labelNew $ Just "+NEW"
  ctxNew <- Gtk.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxNew "new_tab"

  buttonPage <- Gtk.notebookAppendPage notebook table $ Just tabLabel

  -- create all the initial table entries
  mapM_ (\tab ->  do
            let (title, start, dir, command, sendList, cssClass) = tab
            addPane notebook (T.pack title) start dir command sendList (T.pack cssClass)) tabList

  -- do not close app if there are active tabs
  Gtk.onWidgetDeleteEvent toplevel $ \e -> do
    checkExit toplevel sessionName sessionFileName orient notebook
  Gtk.onWidgetDestroy toplevel Gtk.mainQuit

  Gtk.widgetShowAll toplevel

  -- key press signal
  _ <- Gtk.onWidgetKeyPressEvent toplevel $ \e -> do
        liftIO $ forwardKeyPress notebook e
        return True

  -- start the Gtk event loop
  Gtk.main


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
forwardKeyPress :: Gtk.Notebook -> Gdk.EventKey -> IO ()
forwardKeyPress notebook e = do
  pageNumber <- Gtk.notebookGetCurrentPage notebook
  tab <- Gtk.notebookGetNthPage notebook pageNumber
  key <- Gdk.getEventKeyKeyval e
  mods <- Gdk.getEventKeyState e
  case tab of
    Nothing -> return ()
    Just page -> do
      ctr <- castTo Gtk.Container page
      case ctr of
        Nothing -> return ()
        Just ctr -> do
                  n <- Gtk.containerGetChildren ctr
                  let first = head n
                  s <- castTo Gtk.Socket first
                  case s of
                    Nothing -> return ()
                    Just socket -> do
                           socketId <- Gtk.socketGetId socket
                           pw <- Gtk.socketGetPlugWindow socket
                           -- putStrLn $ "key: " ++ (show key) ++ "  mods: " ++ (show mods)
                           case pw of
                             Nothing -> return ()
                             Just pw -> do
                                     -- this might be a solution:
                                     -- ptrw <- unsafeManagedPtrCastPtr pw
                                     -- Gdk.setEventKeyWindow e ptrw
                                     -- e' <- Gdk.toEvent e       -- missing from Gdk3
                                     -- e' <- castTo Gdk.Event e  -- also missing from Gdk3
                                     -- Gdk.eventPut ev
                                     -- hack to fix "less" appearing as "greater"
                                     -- because '>' is 'shift-<' so remove shift from '<'
                                     if key == (fromIntegral (ord '<') :: Word32) then SC.sendKey socket [] key
                                     else SC.sendKey socket mods key

-- compile buttons from current configuration
createButtons :: String -> String -> Gtk.Grid -> Gtk.Notebook -> IO ()
createButtons configFileName cssFileName table notebook = do

  -- erase old contents
  contents <- Gtk.containerGetChildren table
  mapM_ (\item -> Gtk.containerRemove table item) contents

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
              Gtk.widgetShowAll table
              return ()


-- do not allow exit if still some tabs are open
checkExit :: Gtk.Window -> String -> String -> SP.Orientation -> Gtk.Notebook -> IO Bool
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
      pageCount <- Gtk.notebookGetNPages notebook
      tabs <- mapM oneTab [1 .. pageCount - 1]
      let s = SP.Session sessionName orient tabs
      SP.writeSession s sessionFileName
        where
          oneTab i = do
            p <- Gtk.notebookGetNthPage notebook i
            case p of
              Nothing -> return ""
              Just page -> do
                       t <- Gtk.widgetGetName page
                       return $ T.unpack t


-- dialog warning about acive tabs
exitNotice :: Int -> IO ()
exitNotice active = do
  let verb = if active == 1 then "is" else "are"
  let noun = if active == 1 then "tab" else "tabs"
  let message = "<span foreground=\"red\" background=\"white\" size=\"xx-large\">There " ++
                verb ++ " " ++ (show active) ++ " " ++ noun ++ " still active</span>"
  let role = "conlecterm-warning" -- for window manager

  dialog <- Gtk.dialogNew
  Gtk.windowSetRole dialog role
  Gtk.windowSetModal dialog True
  Gtk.windowSetGravity dialog Gdk.GravityCenter

  display <- Gtk.widgetGetDisplay dialog
  w <- Gtk.toWindow dialog
  m <- Gdk.displayGetMonitorAtPoint display 0 0
  r <- Gdk.monitorGetGeometry m
  w <- Gdk.getRectangleWidth r
  h <- Gdk.getRectangleHeight r
  Gtk.windowMove dialog (w `div` 2) (h `div` 2)


  area <- Gtk.dialogGetContentArea dialog
  label <- Gtk.labelNew $ Just ""
  Gtk.labelSetMarkup label $ T.pack message
  Gtk.widgetSetVexpand label True
  Gtk.widgetSetHexpand label True
  Gtk.widgetSetMarginStart label 50
  Gtk.widgetSetMarginEnd label 50
  Gtk.widgetSetMarginTop label 50
  Gtk.widgetSetMarginBottom label 50
  Gtk.widgetShow label
  Gtk.containerAdd area label
  Gtk.dialogAddButton dialog "OK" 42
  Gtk.widgetGrabAdd dialog
  response <- Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()

-- add buttons to the button menu
addButton :: Gtk.Grid -> Int32 -> Int32 -> Gtk.Notebook -> T.Text -> Bool -> Maybe String -> CP.CommandList -> [String] -> T.Text -> IO ()
addButton table x y notebook title autoStart dir commandList sendList cssClass = do
  label <- Gtk.labelNew $ Just title
  ctx1 <- Gtk.widgetGetStyleContext label
  CTX.styleContextAddClass ctx1 "item_button"
  CTX.styleContextAddClass ctx1 cssClass

  button <- Gtk.buttonNew

  Gtk.widgetSetName button title
  Gtk.containerAdd button label

  _ <- Gtk.onButtonClicked button $ (addPane notebook title autoStart dir commandList sendList cssClass >> return ())

  Gtk.widgetShow button
  Gtk.gridAttach table button x y 1 1
  return ()


-- add auto/manual started panes
addPane :: Gtk.Notebook ->  T.Text -> Bool -> Maybe String -> CP.CommandList -> [String] -> T.Text -> IO Int32
addPane notebook title autoStart dir commandList sendList cssClass = do

  vbox <- Gtk.boxNew Gtk.OrientationVertical 0 -- 0 pixel spacing
  Gtk.boxSetHomogeneous vbox False
  Gtk.widgetSetCanFocus vbox False
  Gtk.widgetSetName vbox title
  Gtk.widgetSetVexpand vbox True
  Gtk.widgetShow vbox

  -- create a socket and put it in the Vbox
  socket <- Gtk.socketNew
  Gtk.widgetSetCanFocus socket True
  Gtk.containerAdd vbox socket

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

  buttonBox <- Gtk.gridNew
  Gtk.widgetSetVexpand buttonBox True

  Gtk.setGridColumnHomogeneous buttonBox True
  Gtk.setGridRowHomogeneous buttonBox True

  -- remove tab button
  rtLabel <- Gtk.labelNew $ Just "Remove Tab"
  ctxRtLabel <- Gtk.widgetGetStyleContext rtLabel
  CTX.styleContextAddClass ctxRtLabel "remove_tab_button"

  rtBtn <- Gtk.buttonNew
  ctxRtBtn <- Gtk.widgetGetStyleContext rtBtn
  CTX.styleContextAddClass ctxRtBtn "remove_tab_button"

  Gtk.containerAdd rtBtn rtLabel

  _ <- Gtk.onButtonClicked rtBtn $ closePage notebook vbox
  Gtk.widgetShow rtBtn

  -- start button
  stLabel <- Gtk.labelNew $ Just "Start"
  ctxStLabel <- Gtk.widgetGetStyleContext stLabel
  CTX.styleContextAddClass ctxStLabel "start_button"

  stBtn <- Gtk.buttonNew
  ctxStBtn <- Gtk.widgetGetStyleContext stBtn
  CTX.styleContextAddClass ctxStBtn "start_button"

  Gtk.containerAdd stBtn stLabel

  _ <- Gtk.onButtonClicked stBtn $ press buttonBox socket title refproc dir commandList
  Gtk.widgetShow stBtn

  -- button ordering start top, close bottom
  Gtk.gridAttach buttonBox stBtn startX startY startWidth startHeight
  Gtk.gridAttach buttonBox rtBtn removeX removeY removeWidth removeHeight

  Gtk.containerAdd vbox buttonBox

  -- automatically start sub-process?
  when (not autoStart) $ Gtk.widgetShow buttonBox

  tabLabel <- Gtk.labelNew $ Just title
  ctxTL <- Gtk.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxTL "item_tab"
  CTX.styleContextAddClass ctxTL cssClass
  page <- Gtk.notebookAppendPage notebook vbox $ Just tabLabel

  setTabStopped tabLabel

  -- new page is can be reordered
  Gtk.notebookSetTabReorderable notebook vbox True

  _ <- Gtk.onSocketPlugRemoved socket $ unplug buttonBox tabLabel socket refproc
  _ <- Gtk.onSocketPlugAdded socket $ plug tabLabel socket sendList

  when autoStart $ runC refproc socket title dir commandList

  return page

-- remove a closed page
closePage :: Gtk.Notebook -> Gtk.Box -> IO ()
closePage notebook page = do
  pageNumber <- Gtk.notebookPageNum notebook page
  Gtk.notebookRemovePage notebook pageNumber


-- prevent reorder < 1st place
reordered :: Gtk.Notebook -> Gtk.Widget -> Word32 -> IO ()
reordered notebook page position = do
  when (position < 2) $ Gtk.notebookReorderChild notebook page 1


-- run a command
runC :: PR.ProcRef -> Gtk.Socket -> T.Text -> Maybe String -> CP.CommandList -> IO ()
runC refproc socket title dir commandList = do
  --Gtk.widgetShowAll socket
  Gtk.widgetSetVexpand socket True
  Gtk.widgetShow socket

  -- expand the command string
  paneid <- Gtk.socketGetId socket
  let windowID = toInteger paneid :: Integer
  let cmd = CP.expandCommand commandList windowID (T.unpack title)
  PR.run refproc dir cmd


-- button pressed
press :: Gtk.Grid -> Gtk.Socket -> T.Text -> PR.ProcRef -> Maybe String -> CP.CommandList -> IO ()
press buttons socket title refproc dir commandList = do
  Gtk.widgetHide buttons
  runC refproc socket title dir commandList
  Gtk.widgetGrabFocus socket


-- detect the program creating its main window
-- delay in order to give it time to set itself up
-- send too quickly and the event queue locks up
plug :: Gtk.Label -> Gtk.Socket -> [String] -> IO ()
plug tabLabel socket sendList = do
  _h <- GLib.timeoutAdd GLib.PRIORITY_HIGH 1000 (delayedSend tabLabel socket sendList)
  return ()


-- routine to send the text lines
delayedSend :: Gtk.Label -> Gtk.Socket -> [String] -> IO Bool
delayedSend tabLabel socket sendList = do
  mapM_ (SC.sendLine socket) sendList
  setTabRunning tabLabel
  Gtk.widgetGrabFocus socket
  return False


-- dialog to decide whether to restart the command
unplug :: Gtk.Grid -> Gtk.Label -> Gtk.Socket ->  PR.ProcRef -> IO Bool
unplug otherButtons tabLabel socket refproc = do
  Gtk.widgetHide socket

  PR.shutdown refproc

  --Gtk.widgetShowAll otherButtons
  Gtk.widgetShow otherButtons

  setTabStopped tabLabel

  return True


-- so CSS can set the text colour of a tab label
setTabRunning :: Gtk.Label -> IO ()
setTabRunning tabLabel = do
  ctxTab <- Gtk.widgetGetStyleContext tabLabel
  CTX.styleContextAddClass ctxTab "running"

setTabStopped :: Gtk.Label -> IO ()
setTabStopped tabLabel = do
  ctxTab <- Gtk.widgetGetStyleContext tabLabel
  CTX.styleContextRemoveClass ctxTab "running"


-- change the main title to be the tab name
pageChange :: Gtk.Window -> Gtk.Notebook -> Gtk.Grid -> String -> String -> Gtk.Widget -> Word32 -> IO ()
pageChange window notebook table configFileName cssFileName w pageW = do
  let page = fromInteger (toInteger pageW) :: Int32
  when (page == 0) $ createButtons configFileName cssFileName table notebook
  changeTitle window notebook page


changeTitle :: Gtk.Window -> Gtk.Notebook -> Int32 -> IO ()
changeTitle window notebook page = do
  vBox <- Gtk.notebookGetNthPage notebook page
  case vBox of
    Nothing ->  Gtk.set window [Gtk.windowTitle Gtk.:= initialTitle]

    Just thePage -> do
              text <- Gtk.notebookGetTabLabelText notebook thePage
              let sep = " - " :: T.Text
              let title = case text of
                            Nothing -> initialTitle <> sep <> (T.pack $ show page)
                            Just s  -> initialTitle <> sep <> s
              Gtk.setWindowTitle window title
