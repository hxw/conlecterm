-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module ProcessRunner where

import Data.Maybe
import System.Process

import qualified Control.Concurrent as CC


{-
run :: Maybe String -> [String] -> IO ()
run dir command = do
  tid <- CC.forkIO $ xrun dir command
  CC.yield
  return ()
-}


run :: Maybe String -> [String] -> IO ()
run dir (prog:args) = do
  (_, _, _, proc) <-
    createProcess (proc prog args)
                  { cwd = dir
                  , std_out = Inherit
                  , std_err = Inherit
                  }
  --CC.yield
  --exitStatus <- waitForProcess proc
  return ()
