{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Save.hs

Make the World and Save the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Save (loadFile
                         , saveFile) where

import Control.Monad.Random (getStdGen)
import Data.Aeson
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Directory
import Engine.Arrow.Data (mkWorld, World(..))
import Game.Kind.Entity (EntityKind)
import Game.Kind.Tile ()
import qualified Game.Entity as GE
import qualified Game.Player as GP

width, height :: Int
(width, height) = (640, 480)

-- | loadFile
loadFile :: IO World
loadFile = do
  gen <- getStdGen
  homeDir <- getHomeDirectory
  w <- loadWorld (homeDir ++ saveWorld)
  p <- loadPlayer (homeDir ++ savePlayer)
  let world = if null w
        then mkWorld gen (width, height) 80 50
        else head w
      player = if null p
        then let
        (newPlayer, _) = GP.getPlayer (entityT world)
        in newPlayer
        else head p
      newWorld = world {
        entityT = GE.safeInsertEntity 0 player (gameT world) (entityT world) }
  return newWorld

-- | loadPlayer
loadPlayer :: FilePath -> IO [EntityKind]
loadPlayer fp = do
  touch fp
  p <- eitherDecode <$> C8.readFile fp
  let (_, player) = partitionEithers [p]
  return player

-- | loadWorld
loadWorld :: FilePath -> IO [World]
loadWorld fp = do
  touch fp
  w <- eitherDecode <$> C8.readFile fp
  let (_, world) = partitionEithers [w]
  return world

-- | saveFile
saveFile :: World -> IO ()
saveFile w = do
  let (pEntity, _) = GP.getPlayer (entityT w)
  homeDir <- getHomeDirectory
  encodeFile (homeDir ++ saveWorld) w
  encodeFile (homeDir ++ savePlayer) pEntity

-- | savePlayer
savePlayer :: FilePath
savePlayer = "/Documents/player.json"

-- | saveGame
saveWorld :: FilePath
saveWorld = "/Documents/arrow.json"


-- | touch file in case doesn't exist
touch :: FilePath -> IO ()
touch fp = appendFile fp ""
