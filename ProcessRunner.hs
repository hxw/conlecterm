-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module ProcessRunner where

import Data.Maybe
import System.Process
import Data.IORef
import System.IO.Unsafe( unsafePerformIO )

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as V

type ProcRef = V.MVar ProcessHandle

newProcRef :: IO ProcRef
newProcRef = V.newEmptyMVar

--setProcRef :: ProcRef -> ProcessHandle -> IO ()
--setProcRef = V.putMVar


-- run a command on a procref
run :: ProcRef -> Maybe String -> [String] -> IO ()
run procref dir (prog:args) = do
  (_, _, _, proc) <-
    createProcess (proc prog args)
                  { cwd = dir
                  , std_out = Inherit
                  , std_err = Inherit
                  }
  V.putMVar procref proc
  inc
  return ()


-- ait for child termination
shutdown :: ProcRef -> IO ()
shutdown refproc = do
  f <- V.isEmptyMVar refproc
  if f
    then return ()
    else do
      proc <- V.takeMVar refproc
      dec
      exitStatus <- waitForProcess proc
      return ()


counter = unsafePerformIO $ newIORef 0

inc = do
  i <- readIORef counter
  writeIORef counter $ i + 1

dec = do
  i <- readIORef counter
  writeIORef counter $ i - 1

activeProcs :: IO Int
activeProcs = do
  readIORef counter
