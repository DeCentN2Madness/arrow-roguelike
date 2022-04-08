{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Visual.hs

This module keeps the gylph

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Visual (VisualKind(..)) where

import Prelude hiding (lookup)
import Data.Aeson
import GHC.Generics

data VisualKind
  = VActor
  | VCoin
  | VCorpse
  | VItem
  | VMagma
  | VMouse
  | VMushroom
  | VOpen
  | VPotion
  | VRubble
  | VRock
  | VStairDn
  | VStairUp
  | VTrap
  | VArrow
  | VWall
  | VLWall
  | VLOpen
  | VLRock
  | VLMagma
  | VLRubble
  | VSpider
  | VPerson
  | VPoison
  | VFire
  | VCold
  | VDragon
  | VWolf
  | VSkeleton
  | VOrc
  | VTroll
  | VHuman
  | VDoor
  | V1
  | V2
  deriving (Ord, Show, Eq, Generic)

instance FromJSON VisualKind
instance ToJSON VisualKind
