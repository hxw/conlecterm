
-- Copyright (c) 2012-2025, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import System.Environment (getArgs, getProgName, getEnv)
import System.Console.GetOpt
import System.Exit (exitSuccess, exitFailure)
import System.IO
import System.FilePath (combine)
import Control.Exception (catchJust)
import Control.Monad (filterM)
import System.IO.Error (isDoesNotExistErrorType, ioeGetErrorType)
import System.Posix.Files  (fileExist, isDirectory, getFileStatus)

import qualified TerminalUI as TU

-- to get version from conlecterm.cabal
import Paths_conlecterm (version)
import qualified Data.Version as DV

--constants
currentVersion :: String
currentVersion = DV.showVersion version

defaultSessionFile :: String
defaultSessionFile = "default.session"

configurationFile :: String
configurationFile = "conlecterm.conf"

cssFile :: String
cssFile = "conlecterm.css"


-- main program
main :: IO ()
main = do
  args <- getArgs
  let ( actions, arguments, msgs ) = getOpt RequireOrder options args
  if [] /= msgs then usage msgs else return ()
  opts <- foldl (>>=) defaultOptions actions
  let Options { optConfig = configDirectory
              , optSession = session
              , optVerbose = verbose
              } = opts
  if verbose
    then hPutStrLn stderr "# Verbose mode on"
    else return ()
  if length arguments > 1
    then usage ["extraneous extra arguments\n"]
    else return ()

  let startSession = if length arguments == 0 then defaultSessionFile else (arguments !! 0)

  let configFile = combine configDirectory configurationFile
  let styleFile = combine configDirectory cssFile
  let sessionFile = combine configDirectory $ startSession
  if verbose
    then do
      hPutStrLn stderr $ "requested session:  " ++ (show session)
      hPutStrLn stderr $ "start session:      " ++ (show startSession)
      hPutStrLn stderr $ "configuration file: " ++ configFile
      hPutStrLn stderr $ "CSS file:           " ++ styleFile
      hPutStrLn stderr $ "session file:       " ++ sessionFile
    else return ()

  case configDirectory of
    "" -> usage ["missing configuration directory"]
    _ -> return ()

  haveConfig <- fileExist configFile
  case haveConfig of
    False -> usage ["missing configuration file: ", configFile]
    True -> return ()

  haveStyle <- fileExist styleFile
  case haveStyle of
    False -> usage ["missing CSS file: ", styleFile]
    True -> return ()

  haveSession <- fileExist sessionFile
  case haveSession of
    False -> usage ["missing session file: ", sessionFile]
    True -> return ()

  message <- TU.run configFile styleFile sessionFile verbose
  case message of
    Nothing -> exitSuccess
    Just errorMessage -> usage ["error from Terminal Interface: ", errorMessage]


-- determine configuration directory
-- search for th XDG file first
defaultConfigDirectory :: IO String
defaultConfigDirectory = do
  pn <- getProgName
  let programName = case pn of
        "" ->  "conlecterm"
        n ->  n
  let configDirectories = [ ("XDG_HOME", programName)
                          , ("HOME", combine ".config" programName)
                          , ("HOME", "." ++ programName)
                          ]
  configs <- mapM getConfig configDirectories
  validConfigs <- filterM getDirectoryStatus configs
  case validConfigs of
    [] ->  return ""
    _ ->  return $ head validConfigs


getConfig :: (String, String) -> IO String
getConfig (env, dir) = do
  catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
    (getc2 env dir)
    (\_ -> return "")
  where
    getc2 :: String -> String -> IO String
    getc2 anEnv aDir = do
      path <- getEnv anEnv
      return $ combine path aDir

getDirectoryStatus :: String -> IO Bool
getDirectoryStatus fileName = do
  status <- fileExist fileName
  if status
    then do
      status' <- getFileStatus fileName
      return $ isDirectory status'
    else return False


-- option processing
--------------------

data Options = Options { optVerbose :: Bool
                       , optSession :: String
                       , optConfig :: String
                       }

defaultOptions :: IO Options
defaultOptions = do
  defaultCfg <-  defaultConfigDirectory
  return Options { optVerbose = False
                 , optSession = ""
                 , optConfig = defaultCfg
                 }


options :: [OptDescr (Options -> IO Options)]
options = [ Option ['h'] ["help"]    (NoArg showUsage)           "this message"
          , Option ['v'] ["verbose"] (NoArg setVerbose)          "verbose output"
          , Option ['V'] ["version"] (NoArg showVersion)         "show version number"
          , Option ['c'] ["config"]  (ReqArg setConfig "DIR")    "configuration directory"
--          , Option ['c'] ["config"]  (ReqArg readInput "FILE")   "configuration file"
--          , Option ['o'] ["output"]  (ReqArg writeOutput "FILE") "output file to write"
          , Option ['s'] ["session"] (ReqArg setSession "FILE")  "session file"
          ]


-- option handlers
------------------

setVerbose :: Monad m => Options -> m Options
setVerbose opt = return opt { optVerbose = True }

--setTarget opt = return opt { optTarget = True }

setConfig :: Monad m => String -> Options -> m Options
setConfig arg opt = return opt { optConfig = arg }

setSession :: Monad m => String -> Options -> m Options
setSession arg opt = return opt { optSession = arg }


-- usage messages
-----------------

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn currentVersion
  exitSuccess

showUsage :: Options -> IO Options
showUsage _ = do
  usage []
  exitFailure

usage :: [String] -> IO ()
usage errors = do
  progname <- getProgName
  let header = "usage: " ++ progname ++ " [option...] session"
  hPutStrLn stderr $ concat errors ++ "\n" ++ usageInfo header options
  exitFailure
