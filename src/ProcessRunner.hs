-- Copyright (c) 2012-2019, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

module ProcessRunner where

import System.Process
import Data.IORef
import System.IO.Unsafe( unsafePerformIO )
import System.IO( hPutStrLn, stderr )
import Control.Exception( catch, IOException )

import qualified Control.Concurrent.MVar as V
import qualified System.Directory as SD

type ProcRef = V.MVar ProcessHandle

newProcRef :: IO ProcRef
newProcRef = V.newEmptyMVar

-- run a command on a procref
--
-- do not crash on error, though a process failure seems to make the tab unusable
-- if the dir was removed then simply start in the conlecterm start dir
run :: ProcRef -> Maybe String -> [String] -> IO ()
run _ _ [] = do
  _ <- error "ProcessRunner.run: empty run command"
  return ()
run procref dir (prog:args) = do
  catch (doRun)
            (\e -> do
               let err = show (e :: IOException)
               _ <- hPutStrLn stderr ("ProcessRunner.run: error: " ++ err)
               return ()
            )
      where
        doRun = do
            checkedDirectory <- checkDir dir
            (_, _, _, aProc) <-
                createProcess (proc prog args)
                                  { cwd = checkedDirectory
                                  , std_out = Inherit
                                  , std_err = Inherit
                                  }
            V.putMVar procref aProc
            inc
            return ()

        checkDir Nothing = return Nothing
        checkDir (Just dir) = do
            f <- SD.doesDirectoryExist dir
            if f
            then do
              _ <- hPutStrLn stderr ("ProcessRunner.run: directory disappeared: " ++ dir)
              return $ Just dir
            else return Nothing

-- ait for child termination
shutdown :: ProcRef -> IO ()
shutdown refproc = do
  f <- V.isEmptyMVar refproc
  if f
    then return ()
    else do
      aProc <- V.takeMVar refproc
      dec
      _exitStatus <- waitForProcess aProc
      return ()


counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

inc :: IO ()
inc = do
  i <- readIORef counter
  writeIORef counter $ i + 1

dec :: IO ()
dec = do
  i <- readIORef counter
  writeIORef counter $ i - 1

activeProcs :: IO Int
activeProcs = do
  readIORef counter
