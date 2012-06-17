-- Copyright (c) 2012, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

module ProcessRunner where

import Data.Maybe
import System.Process



run :: Maybe String -> [String] -> IO ()
run dir (prog:args) = do
  (_, _, _, _) <-
    createProcess (proc prog args)
                  { cwd = dir
                  , std_out = Inherit
                  , std_err = Inherit
                  }
  return ()
