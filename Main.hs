-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module Main where

import System.Environment( getArgs, getProgName, getEnv )
import System.Console.GetOpt
import System.Exit( exitSuccess, exitFailure )
import System.IO
import System.FilePath( combine, pathSeparator )
import Control.Exception( tryJust )
import System.IO.Error( isDoesNotExistError )

import qualified ConfigurationParser as CP
import qualified TerminalUI as TU


--constants
currentVersion = "Version 1"
defaultSession = ""


defaultConfigFile :: IO String
defaultConfigFile = do
  progname <- getProgName
  case progname of
    "" ->
      return ".conlectermrc"
    n ->
      return $ "." ++ n ++ "rc"


-- main program
main :: IO ()
main = do
  args <- getArgs
  let ( actions, arguments, msgs ) = getOpt RequireOrder options args
  if [] /= msgs then usage msgs else return ()
  opts <- foldl (>>=) defaultOptions actions
  let Options { optConfig = config
              , optSession = session
              , optVerbose = verbose
              } = opts
  if verbose
    then hPutStrLn stderr "# Verbose mode on"
    else return ()
  if length arguments > 1
    then usage ["too many arguments\n"]
    else return ()
  hPutStrLn stderr $ "default session = " ++ (show session) ++ "   config = " ++ config

  let startSession = if length arguments == 0 then defaultSession else (arguments !! 0)
  putStrLn $ "r = " ++ (show startSession)
  mh <- CP.compile config
  case mh of
    Nothing ->
      exitFailure
    Just h -> do
      session <- CP.expandSession h startSession
      case session of
        Nothing -> do
          putStrLn $ "unknown session: " ++ (show startSession)
          exitFailure
        Just s ->
          TU.run s
  exitSuccess


-- get the absolute path of a file
-- no change if already absolute
-- if HOME is set use it as the prefix otherwise use the root directory
configPath :: String -> IO String
configPath p = do
  result <- tryJust justDoesNotExistError $ getEnv "HOME"
  case result of
    Left path  -> return $ combine path p
    Right path -> return $ combine path p
  where
    justDoesNotExistError :: IOError -> Maybe String
    justDoesNotExistError e
        | isDoesNotExistError e = Just [pathSeparator]
        | otherwise             = Nothing




-- option processing
--------------------

data Options = Options { optVerbose :: Bool
                       , optSession :: String
                       , optConfig :: String
                       }

defaultOptions :: IO Options
defaultOptions = do
  defaultCfg <-  defaultConfigFile
  cfg <- configPath defaultCfg
  return Options { optVerbose = False
                 , optSession = ""
                 , optConfig = cfg
                 }


options :: [OptDescr (Options -> IO Options)]
options = [ Option ['h'] ["help"]    (NoArg showUsage)           "this message"
          , Option ['v'] ["verbose"] (NoArg setVerbose)          "verbose output"
          , Option ['V'] ["version"] (NoArg showVersion)         "show version number"
          , Option ['c'] ["config"]  (ReqArg setConfig "FILE")   "configuration file"
--          , Option ['c'] ["config"]  (ReqArg readInput "FILE")   "configuration file"
--          , Option ['o'] ["output"]  (ReqArg writeOutput "FILE") "output file to write"
          , Option ['s'] ["session"] (ReqArg setSession "NAME")  "session name"
          ]


-- option handlers
------------------

setVerbose opt = return opt { optVerbose = True }
--setTarget opt = return opt { optTarget = True }
setConfig arg opt = return opt { optConfig = arg }
setSession arg opt = return opt { optSession = arg }
--readInput arg opt = return opt { optInput = readFile arg }
--writeOutput arg opt = return opt { optOutput = writeFile arg }
--writeOutput arg opt = return opt { optOutput = withFile arg WriteMode }
--writeOutput arg opt = do
--  handle <- openFile arg WriteMode
--  return opt { optOutput = handle }


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
  hPutStrLn stderr $ concat errors ++ usageInfo header options
  exitFailure
