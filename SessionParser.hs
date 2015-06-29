-- Copyright (c) 2012-2015, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD see LICENSE.text

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module SessionParser where

import Data.Aeson
--import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import GHC.Generics


-- import Data.Maybe( isNothing, fromMaybe, fromJust )
-- import Data.Foldable( foldlM )
-- import Text.Parsec.Prim( ParsecT )
-- import Text.ParserCombinators.Parsec
-- import qualified Text.ParserCombinators.Parsec.Token as P
-- import qualified Text.Parsec.Prim as N
-- import Text.ParserCombinators.Parsec.Language

-- import qualified Graphics.UI.Gtk as GTK

-- import System.IO
-- import System.Posix.Files ( fileExist )
-- import System.FilePath( combine, pathSeparator )
-- import System.Environment( getEnv )

-- import qualified Data.HashTable.IO as HT
-- import Control.Monad.Trans (liftIO, lift)
-- import qualified Text.Printf as TP
-- import qualified System.Directory as SD


data Orientation = LeftTabs | RightTabs | TopTabs | BottomTabs
                   deriving Show

data Session =
    Session { name        :: !String
            , orientation :: !Orientation
            , tabs        :: ![String]
            } deriving (Show, Generic)

instance FromJSON Session
instance ToJSON Session


-- conversion of Orientation to and from JSON

toOrientation "top" = TopTabs
toOrientation "bottom" = BottomTabs
toOrientation "left" = LeftTabs
toOrientation "right" = RightTabs
toOrientation _ = error "Invalid orientation"

fromOrientation TopTabs = "top"
fromOrientation BottomTabs = "bottom"
fromOrientation LeftTabs = "left"
fromOrientation RightTabs = "right"

instance FromJSON Orientation where
    parseJSON (String v) = return $ toOrientation v
    parseJSON _ = mzero

instance ToJSON Orientation where
    toJSON tblr = fromOrientation tblr


readSession :: String -> IO (Maybe Session)
readSession filename = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String Session)
  case d of
    -- error with string
    Left err -> do
           putStrLn err
           return Nothing
    -- success with Session
    Right s -> return $ Just s
  where
    getJSON = B.readFile filename


writeSession :: Session -> String -> IO ()
writeSession session fileName =
    putJSON $ encode session
        where
          putJSON s = B.writeFile fileName s
