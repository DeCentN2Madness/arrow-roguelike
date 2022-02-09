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
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Directory
import Engine.Arrow.Data (World)
import Game.Kind.Entity ()
import Game.Kind.Tile ()

-- | loadFile
loadFile :: FilePath -> IO (Either String World)
loadFile fp = do
  homeDir <- getHomeDirectory
  eitherDecode <$> L8.readFile (homeDir ++ fp)

-- | saveFile
saveFile :: FilePath -> World -> IO ()
saveFile fp w = do
  homeDir <- getHomeDirectory
  encodeFile (homeDir ++ fp) w

saveGame :: FilePath
saveGame = "/Documents/arrow.json"
