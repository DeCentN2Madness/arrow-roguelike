{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.Save.hs

Make the World and Save the World

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Save (loadFile, saveFile) where

import Control.Monad.Random (getStdGen)
import Data.Aeson
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Directory
import Engine.Arrow.Data (mkWorld, World(..))
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind)
import Game.Kind.Tile ()
import qualified Game.Player as GP

width, height :: Int
(width, height) = (1024, 768)

-- | loadFile
-- Expected: $HOME/Documents/Arrow
loadFile :: IO World
loadFile = do
  gen <- getStdGen
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False (homeDir ++ "/Arrow")
  w <- loadWorld (homeDir ++ saveWorld)
  p <- loadPlayer (homeDir ++ savePlayer)
  e <- loadAsset (homeDir ++ saveEntity)
  let world = if null w
        then mkWorld gen (width, height) 80 50
        else head w
      player = if null p
        then let
        (pEntity, _) = GP.getPlayer (entityT world)
        in pEntity
        else head p
      asset = if null e
        then assetT world
        else head e
      newWorld = world { assetT  = asset
                       , entityT = GE.safeInsertEntity 0 player (gameT world) (entityT world) }
  return newWorld

-- | loadAsset -- all the stuff
loadAsset :: FilePath -> IO [EntityMap]
loadAsset fp = do
  touch fp
  e <- eitherDecode <$> C8.readFile fp
  let (err, load) = partitionEithers [e]
  print $ fp ++ ", " ++ show err
  return load

-- | loadPlayer -- the Hero '@'
loadPlayer :: FilePath -> IO [EntityKind]
loadPlayer fp = do
  touch fp
  e <- eitherDecode <$> C8.readFile fp
  let (err, load) = partitionEithers [e]
  print $ fp ++ ", " ++ show err
  return load

-- | loadWorld -- the World
loadWorld :: FilePath -> IO [World]
loadWorld fp = do
  touch fp
  w <- eitherDecode <$> C8.readFile fp
  let (err, world) = partitionEithers [w]
  print $ fp ++ ", " ++ show err
  return world

-- | saveFile
saveFile :: World -> IO ()
saveFile w = do
  let (pEntity, _) = GP.getPlayer (entityT w)
  homeDir <- getHomeDirectory
  encodeFile (homeDir ++ saveWorld) w
  encodeFile (homeDir ++ savePlayer) pEntity
  encodeFile (homeDir ++ saveEntity) (assetT w)

-- | saveEntity
saveEntity :: FilePath
saveEntity = "/Documents/Arrow/entity.json"

-- | savePlayer
savePlayer :: FilePath
savePlayer = "/Documents/Arrow/player.json"

-- | saveGame
saveWorld :: FilePath
saveWorld = "/Documents/Arrow/arrow.json"


-- | touch file in case doesn't exist
touch :: FilePath -> IO ()
touch fp = appendFile fp ""
