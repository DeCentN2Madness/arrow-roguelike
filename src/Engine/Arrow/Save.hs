{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Save.hs

Save the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Save (loadFile
                         , saveFile
                         , saveGame) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Directory
import Engine.Arrow.Data (World)
import Game.Kind.Entity ()
import Game.Kind.Tile ()

-- | loadFile
loadFile :: FilePath -> IO (Either String World)
loadFile fp = do
  homeDir <- getHomeDirectory
  touch (homeDir ++ fp)
  eitherDecode <$> C8.readFile (homeDir ++ fp)

-- | saveFile
saveFile :: FilePath -> World -> IO ()
saveFile fp w = do
  homeDir <- getHomeDirectory
  encodeFile (homeDir ++ fp) w

-- | saveGame
saveGame :: FilePath
saveGame = "/Documents/arrow.json"

-- | touch file in case doesn't exist
touch :: FilePath -> IO ()
touch fp = appendFile fp ""
