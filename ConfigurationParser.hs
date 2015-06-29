-- Copyright (c) 2012-2015, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ConfigurationParser where

import Data.Maybe( isNothing, fromMaybe, fromJust )
import Data.Foldable( foldlM )
import Data.List( sortBy )
import Data.Text( pack, toLower )
import Text.Parsec.Prim( ParsecT )
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.Parsec.Prim as N
import Text.ParserCombinators.Parsec.Language

import qualified Graphics.UI.Gtk as GTK

import System.IO
import System.Posix.Files ( fileExist )
import System.FilePath( combine, pathSeparator )
import System.Environment( getEnv )

import qualified Data.HashTable.IO as HT
import Control.Monad.Trans (liftIO, lift)
import qualified Text.Printf as TP
import qualified System.Directory as SD


-- a command is a list of Command Items
-- the first one being the program
data CommandItem = Argument String
                 | Title    String
                 | Window   String
                 deriving Show

type CommandList = [CommandItem]

type SendList  = [String]

type Colour = GTK.Color

data PaneRecord =
  PaneRecord { paneTitle   :: String
             , paneAuto    :: Bool
             , paneDir     :: Maybe String
             , paneRun     :: String
             , paneSend    :: SendList
             , paneRunning :: Maybe Colour
             , paneStopped :: Maybe Colour
             } deriving Show


type CommandHash = HT.BasicHashTable String CommandList
type PaneHash = HT.BasicHashTable String PaneRecord

type Hashes = (CommandHash, PaneHash)

data UserState =
  UserState { usCommands :: CommandHash
            , usPanes    :: PaneHash
            , usErrorCount   :: Integer
            , usWarningCount :: Integer
            , usCurrentPaneStart   :: Maybe Bool
            , usCurrentPaneRun     :: Maybe String
            , usCurrentPaneDir     :: Maybe String
            , usCurrentPaneSend    :: SendList
            , usCurrentPaneRunning :: Maybe Colour
            , usCurrentPaneStopped :: Maybe Colour
            }

initUserState cmdHT paneHT =
  UserState { usCommands = cmdHT
            , usPanes    = paneHT
            , usErrorCount   = 0
            , usWarningCount = 0
            , usCurrentPaneStart   = Nothing
            , usCurrentPaneRun     = Nothing
            , usCurrentPaneDir     = Nothing
            , usCurrentPaneSend    = []
            , usCurrentPaneRunning = Nothing
            , usCurrentPaneStopped = Nothing
            }


type MyParser =  ParsecT [Char] UserState IO

type MyTokenParser st = P.GenTokenParser String st IO

type MyLanguageDef st = GenLanguageDef String st IO


lexer :: MyTokenParser UserState
lexer  = P.makeTokenParser
         LanguageDef
         { commentStart   = "(*"
         , commentEnd     = "*)"
         , commentLine    = "#"
         , nestedComments = True
         , identStart     = letter
         , identLetter    = alphaNum <|> oneOf "_-"
         , reservedNames  = [ "auto"
                            , "color"
                            , "colour"
                            , "command"
                            , "cwd"
                            , "default"
                            , "manual"
                            , "pane"
                            , "run"
                            , "running"
                            , "start"
                            , "stopped"
                            , "send-line"
                            , "tab-name"
                            , "window-id"
                            ]
         , opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
         , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
         , reservedOpNames= [ "*" ]
         , caseSensitive  = False
         }


whiteSpace    = P.whiteSpace lexer
lexeme        = P.lexeme lexer
symbol        = P.symbol lexer
stringLiteral = P.stringLiteral lexer
natural       = P.natural lexer
hexadecimal   = P.hexadecimal lexer
parens        = P.parens lexer
braces        = P.braces lexer
comma         = P.comma lexer
semi          = P.semi lexer
identifier    = P.identifier lexer
reserved      = P.reserved lexer
reservedOp    = P.reservedOp lexer


-- diagnostic messages
-- -------------------

dupDef :: SourcePos -> Maybe a -> String ->  MyParser ()
dupDef pos item name =
  if isNothing item
    then return ()
    else posWarning pos $ "duplicate definition of: " ++ name

notDef :: SourcePos -> Maybe a -> String ->  MyParser ()
notDef pos item name =
  if isNothing item
    then posError pos $ "undefined symbol: " ++ name
    else return ()


warning :: String ->  MyParser ()
warning message = do
  pos <- getPosition
  posWarning pos message
posWarning :: SourcePos -> String ->  MyParser ()
posWarning pos message = do
  let l = show $ sourceLine pos
  let c = show $ sourceColumn pos
  let n = show $ sourceName pos  -- adds " around string
  lift $ putStrLn $ "warning at " ++ n ++ " (line " ++ l ++ ", column " ++ c ++ "):\n" ++ message
  s <- getState
  let UserState { usWarningCount = n } = s
  let sNew = s { usWarningCount = n + 1 }
  setState sNew


printError :: String ->  MyParser ()
printError message = do
  pos <- getPosition
  posError pos message
posError :: SourcePos -> String ->  MyParser ()
posError pos message = do
  let l = show $ sourceLine pos
  let c = show $ sourceColumn pos
  let n = show $ sourceName pos  -- adds " around string
  lift $ putStrLn $ "error at " ++ n ++ " (line " ++ l ++ ", column " ++ c ++ "):\n" ++ message
  s <- getState
  let UserState { usErrorCount = n } = s
  let sNew = s { usErrorCount = n + 1 }
  setState sNew


compiledOK :: MyParser Bool
compiledOK = do
  s <- getState
  let UserState { usWarningCount = w
                , usErrorCount   = e } = s
  if w /= 0
    then lift $ putStrLn $ "total warnings: " ++ (show w)
    else return ()
  if e /= 0
    then lift $ putStrLn $ "total errors:   " ++ (show e)
    else return ()
  return $ e == 0


-- parser main entry point
-- -----------------------

configParser :: MyParser Bool
configParser = do
  r <- many (commandParser <|> paneParser)
  e <- compiledOK
  return e


-- command blocks
-- --------------

lookupCommand :: String -> MyParser (Maybe CommandList)
lookupCommand name = do
  s <- getState
  let UserState { usCommands = hCommand } = s
  lift $ HT.lookup hCommand name


commandParser :: MyParser ()
commandParser = do
  reserved "command"
  pos <- getPosition
  name <- identifier
  l <- lookupCommand name
  dupDef pos l name

  cmds <- braces $ many commandItem

  s <- getState
  let UserState { usCommands = hCommand } = s
  lift $ HT.insert hCommand name cmds


commandItem :: MyParser CommandItem
commandItem =
    do{ reserved "window-id"
      ; s <- parens stringLiteral
      ; return $ Window s
      }
  <|>  do{ reserved "tab-name"
      ; s <- parens stringLiteral
      ; return $ Title s
      }
  <|> do{ s <- stringLiteral
      ; return $ Argument s
      }
  <?> "command item"


-- pane blocks
-- -----------

paneLookup :: String -> MyParser (Maybe PaneRecord)
paneLookup title = do
  s <- getState
  let UserState { usPanes = hPane } = s
  lift $ HT.lookup hPane title


paneSetup :: String -> MyParser SourcePos
paneSetup title = do
  blockStartPos <- getPosition
  s <- getState
  let sNew = s { usCurrentPaneStart = Nothing
               , usCurrentPaneDir   = Nothing
               , usCurrentPaneRun   = Nothing
               , usCurrentPaneSend  = []
               , usCurrentPaneRunning = Nothing
               , usCurrentPaneStopped = Nothing
               }
  setState sNew

  l <- paneLookup title
  dupDef blockStartPos l title
  return blockStartPos


paneCompile :: SourcePos -> String -> MyParser ()
paneCompile pos title = do
  l <- paneLookup title
  case l of
    Nothing -> do
      s <- getState
      let UserState { usPanes = hPane
                    , usCurrentPaneStart = start
                    , usCurrentPaneDir   = dir
                    , usCurrentPaneRun   = run
                    , usCurrentPaneSend  = send
                    , usCurrentPaneRunning = running
                    , usCurrentPaneStopped = stopped
                    } = s

      let pane = PaneRecord { paneTitle = title
                            , paneAuto  = fromMaybe True start
                            , paneRun   = fromJust run
                            , paneDir   = dir
                            , paneSend  = send
                            , paneRunning = running
                            , paneStopped = stopped
                            }

      lift $ HT.insert hPane title pane

    Just _  -> do
      return ()


paneParser :: MyParser ()
paneParser = do
  reserved "pane"
  title <- stringLiteral
  state <- paneSetup title
  cmds <- braces $ many paneItem

  paneCompile state title


runSetup :: MyParser SourcePos
runSetup = do
  s <- getState
  blockStartPos <- getPosition
  let UserState { usCurrentPaneRun = n } = s
  if isNothing n then return ()
    else warning "duplicate run option"
  return blockStartPos


runCompile :: SourcePos -> String -> MyParser ()
runCompile pos name = do
  l <- lookupCommand name
  notDef pos l name
  case l of
    Nothing -> return ()
    Just _  -> do
      s <- getState
      let sNew = s { usCurrentPaneRun = Just name }
      setState sNew


cwdSetup :: MyParser SourcePos
cwdSetup = do
  s <- getState
  blockStartPos <- getPosition
  let UserState { usCurrentPaneDir = n } = s
  if isNothing n then return ()
    else warning "duplicate cwd option"
  return blockStartPos


cwdCompile :: SourcePos -> String -> MyParser ()
cwdCompile pos dirName = do
  absDirName <- lift $
               case () of _
                             | dirName == "" -> getEnv "HOME"
                             | (head dirName) == pathSeparator -> return dirName
                             | otherwise -> do
                                  path <- getEnv "HOME"
                                  return $ combine path dirName

  f <- lift $ SD.doesDirectoryExist absDirName
  case f of
    False -> posError pos $ "directory: " ++ (show absDirName) ++ " does not exist"
    True  -> do
      s <- getState
      let sNew = s { usCurrentPaneDir = Just absDirName }
      setState sNew


startSetup :: MyParser SourcePos
startSetup = do
  s <- getState
  blockStartPos <- getPosition
  let UserState { usCurrentPaneStart = n } = s
  if isNothing n then return ()
    else warning "duplicate start option"
  return blockStartPos


startCompile :: SourcePos -> Bool -> MyParser ()
startCompile pos flag = do
  s <- getState
  let sNew = s { usCurrentPaneStart = Just flag }
  setState sNew


sendSetup :: MyParser SourcePos
sendSetup = do
  blockStartPos <- getPosition
  return blockStartPos


sendCompile :: SourcePos -> String -> MyParser ()
sendCompile pos str = do
  s <- getState
  let UserState { usCurrentPaneSend = strList } = s
  let sNew = s { usCurrentPaneSend = strList ++ [str] }
  setState sNew


runningSetup :: MyParser SourcePos
runningSetup = do
  s <- getState
  blockStartPos <- getPosition
  let UserState { usCurrentPaneRunning = n } = s
  if isNothing n then return ()
    else warning "duplicate running option"
  return blockStartPos


runningCompile :: SourcePos -> Colour -> MyParser ()
runningCompile pos colour = do
  s <- getState
  let sNew = s { usCurrentPaneRunning = Just colour }
  setState sNew


stoppedSetup :: MyParser SourcePos
stoppedSetup = do
  blockStartPos <- getPosition
  s <- getState
  let UserState { usCurrentPaneStopped = n } = s
  if isNothing n then return ()
    else warning "duplicate stopped option"
  return blockStartPos


stoppedCompile :: SourcePos -> Colour -> MyParser ()
stoppedCompile pos colour = do
  s <- getState
  let sNew = s { usCurrentPaneStopped = Just colour }
  setState sNew



paneItem :: MyParser ()
paneItem =
  do
    reserved "run"
    state <- runSetup
    r <- identifier
    runCompile state r
  <|> do
    reserved "cwd"
    state <- cwdSetup
    r <- stringLiteral
    cwdCompile state r
  <|> do
    reserved "start"
    state <- startSetup
    f <- ((reserved "auto" >> return True) <|> (reserved "manual" >> return False))
    startCompile state f
  <|> do
    reserved "send-line"
    state <- sendSetup
    s <- stringLiteral
    sendCompile state s
  <|> do
    reserved "running"
    state <- runningSetup
    c <- colourItem
    runningCompile state c
  <|> do
    reserved "stopped"
    state <- stoppedSetup
    c <- colourItem
    stoppedCompile state c
  <?> "pane item"


colourItem :: MyParser Colour
colourItem = do
  (reserved "colour" <|> reserved "color")
  parens triplet
  where
    triplet = do
      red <- natural
      comma
      green <- natural
      comma
      blue <- natural
      return $ GTK.Color (fromIntegral red) (fromIntegral green) (fromIntegral blue)


-- parser setup and run
-- --------------------

--runParserT :: Stream s m t => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
--runParser :: Stream s Identity t => Parsec s u a -> u -> SourceName -> s -> Either ParseError a

--runParser :: GenParser tok st a -> st -> SourceName -> [tok] -> Either ParseError a

--run :: Show a => MyParser a -> String -> IO ()
run :: MyParser Bool -> Hashes -> String -> String -> IO (Maybe Hashes)
run p (hashCmd, hashPane) fileName input = do

  let initialState = initUserState hashCmd hashPane

  result <- N.runParserT p initialState fileName input
  --case (N.runParserT p initialState fileName input) of
  case result of
    Left err -> do
      putStr "parse error at "
      print err
      return Nothing
    Right compiledOK -> do
      if compiledOK
        then return $ Just (hashCmd, hashPane)
        else return Nothing


runLex :: MyParser Bool -> Hashes -> String -> String -> IO (Maybe Hashes)
runLex p hashes fileName input =
  run (do{ whiteSpace
         ; x <- p
         ; eof
         ; return x
         }) hashes fileName input


-- entry points
-- ------------

-- compile a configuration file
compile :: [String] -> IO (Maybe Hashes)
compile configFileNames = do
  hashCmd <- HT.new :: IO CommandHash
  hashPane <- HT.new :: IO PaneHash
  let hashes = (hashCmd, hashPane)
  result <- foldlM compileOne hashes configFileNames
  return $ Just result

compileOne :: Hashes -> String -> IO Hashes
compileOne hashes configFileName = do
  flag <- fileExist configFileName
  case flag of
    False -> return hashes
    True -> withFile configFileName ReadMode (process configFileName)
  where
    process :: String -> Handle -> IO Hashes
    process name input = do
      s <- hGetContents input
      result <- runLex configParser hashes name s
      case result of
        Nothing -> return hashes
        Just updateHashes -> return updateHashes
{-
  case parse configFile name s of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ print r
-}


-- simple tuple type for returning the expanded session
type PaneInfo = (String, Bool, Maybe String, CommandList, SendList, Maybe Colour, Maybe Colour)


-- get a list of panes from a list of tab names
expandPanes :: Hashes -> [String] -> IO [PaneInfo]
expandPanes (hashCmd, hashPane) tabs = do
  mapM expandPane tabs
      where
        expandPane :: String -> IO PaneInfo
        expandPane pane = do
          p <- HT.lookup hashPane pane
          let PaneRecord { paneTitle = title
                         , paneAuto  = start
                         , paneDir   = dir
                         , paneRun   = run
                         , paneSend  = send
                         , paneRunning = running
                         , paneStopped = stopped
                         } = fromJust p
          command <- HT.lookup hashCmd run
          return $ (title, start, dir, fromJust command, send, running, stopped)

-- get a sorted list of all the tab names
-- (case insensitive sort)
sortedTabs :: Hashes -> IO [PaneInfo]
sortedTabs (hashCmd, hashPane) = do
    l <- HT.toList hashPane
    let (titles, _) = unzip l
    expandPanes (hashCmd, hashPane) $ sortBy (\a b -> (lc a) `compare` (lc b)) titles
        where
          lc s = toLower $ pack s


-- take a command list and convert to a list of strings
-- expanding the integer value provided

expandCommand :: CommandList -> Integer -> String -> [String]
expandCommand commandList int str =
  map (expandArg int str) commandList


expandArg int str (Argument s) = s
expandArg int str (Title    s) = TP.printf s str
expandArg int str (Window   s) = TP.printf s int
