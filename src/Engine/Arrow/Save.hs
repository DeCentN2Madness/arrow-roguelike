{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Save.hs

Save the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Save (saveFile, loadFile) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Directory
import Engine.Arrow.Data (World)
import Game.Dungeon ()
import Game.Kind.Entity ()
import Game.Kind.Tile ()

-- | saveFile
saveFile :: FilePath -> World -> IO ()
saveFile fp w = do
  homeDir <- getHomeDirectory
  encodeFile (homeDir ++ "/Documents/" ++ fp) w

-- | loadFile
loadFile :: FilePath -> IO (Either String World)
loadFile fp = do
  d <- eitherDecode <$> L8.readFile fp
  return $ case d of
    Left err -> Left err
    Right w  -> Right w
