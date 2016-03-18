-- Copyright (c) 2012-2015, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module ProcessRunner where

import System.Process
import Data.IORef
import System.IO.Unsafe( unsafePerformIO )

import qualified Control.Concurrent.MVar as V

type ProcRef = V.MVar ProcessHandle

newProcRef :: IO ProcRef
newProcRef = V.newEmptyMVar


-- run a command on a procref
run :: ProcRef -> Maybe String -> [String] -> IO ()
run procref dir (prog:args) = do
  (_, _, _, aProc) <-
    createProcess (proc prog args)
                  { cwd = dir
                  , std_out = Inherit
                  , std_err = Inherit
                  }
  V.putMVar procref aProc
  inc
  return ()
run _ _ [] = do
  _ <- error "empty run command"
  return ()


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
