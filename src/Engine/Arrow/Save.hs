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
import Engine.Arrow.Data (World(..))
import qualified Engine.Arrow.Data as EAD
import qualified Game.DiceSet as GDS
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))
import Game.Kind.Tile ()
import qualified Game.Player as GP

width, height :: Int
(width, height) = (1024, 768)

-- | loadFile
-- Expected: $HOME/Documents/Arrow
loadFile :: IO World
loadFile = do
  gen <- getStdGen
  let (seed, _) = GDS.d1000 gen
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False (homeDir ++ "/Documents/Arrow")
  -- copyFile (homeDir ++ sourceAsset) (homeDir ++ saveAsset)
  e <- loadAsset (homeDir ++ saveAsset)
  p <- loadPlayer (homeDir ++ savePlayer)
  w <- loadWorld (homeDir ++ saveWorld)
  let asset = if null e
        then assetT world
        else head e
      player = if null p
        then fst $ GP.getPlayer (entityT world)
        else head p
      world = if null w
        then EAD.mkWorld seed (width, height) 0 80 40
        else head w
      alive = if eHP player > 1
        then player
        else fst $ GP.getPlayer (entityT world)
  return world { assetT  = asset
               , entityT = GE.safeInsertEntity 0 alive (gameT world) (entityT world) }

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
  encodeFile (homeDir ++ saveAsset) (assetT w)
  encodeFile (homeDir ++ saveEntity) (entityT w)

-- | saveAsset
saveAsset :: FilePath
saveAsset = "/Documents/Arrow/asset.json"

-- | saveAsset
saveEntity :: FilePath
saveEntity = "/Documents/Arrow/entity.json"

-- | savePlayer
savePlayer :: FilePath
savePlayer = "/Documents/Arrow/player.json"

-- | saveGame
saveWorld :: FilePath
saveWorld = "/Documents/Arrow/world.json"

-- | touch file in case doesn't exist
touch :: FilePath -> IO ()
touch fp = appendFile fp ""
