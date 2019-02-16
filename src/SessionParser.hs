-- Copyright (c) 2012-2019, Christopher Hall <hsw@ms2.hinet.net>
-- Licence BSD2 see LICENSE file

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module SessionParser where

import Data.Aeson
--import Data.Aeson.Encode.Pretty (encodePretty)

import Data.Text
import Data.String

import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import GHC.Generics


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

toOrientation :: Text -> Orientation
toOrientation "top" = TopTabs
toOrientation "bottom" = BottomTabs
toOrientation "left" = LeftTabs
toOrientation "right" = RightTabs
toOrientation _ = error "Invalid orientation"

fromOrientation :: Data.String.IsString a => Orientation -> a
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
