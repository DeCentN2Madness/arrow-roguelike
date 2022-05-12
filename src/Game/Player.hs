{-# LANGUAGE OverloadedStrings #-}
{-

Game.Player.hs

Game.Player is the engine for the Actor EntityKind. This is used
as interchangable helper functions for id=0

Example: getPlayer returns the Player from the EntityMap

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Player (armorShield
                   , characterEquipment
                   , characterExamine
                   , characterInventory
                   , characterLook
                   , characterSheet
                   , characterStore
                   , getArrow
                   , getHealth
                   , getMana
                   , getMushroom
                   , getPotion
                   , getPlayer
                   , updatePlayerBy
                   , updatePlayer
                   , updatePlayerXP) where

import Prelude hiding (lookup)
import Control.Arrow ((&&&))
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Game.Entity (EntityMap)
import qualified Game.Entity as GE
import Game.Kind.Entity (EntityKind(..))

type Coord = (Int, Int)
type Player = EntityKind
type AssetMap = EntityMap
type Properties = Map Text Text

-- | abilityGain
-- gain at levels 4, 8, 12, 16, 19
abilityGain :: Int -> Int -> (Int, Int)
abilityGain lvl curr
  | lvl == 4  && lvl > curr = (2, 0)
  | lvl == 8  && lvl > curr = (2, 0)
  | lvl == 12 && lvl > curr = (0, 2)
  | lvl == 16 && lvl > curr = (1, 1)
  | lvl == 19 && lvl > curr = (1, 1)
  | otherwise = (0,0)

-- | abilityMod
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

-- | armorShield
-- Armor and Shields modify '@' stats
--   1. ArmorClass (AC)
--   2. Weight (WT)
--   3. Weapon WT (WWT)
--   4. ATTACK, SHOOT Damage
armorShield :: Properties -> AssetMap -> Properties
armorShield pProp am = let
  descMap = Map.fromList $
    [ (name, desc) | (_, v) <- Map.toList am,
      let name = Map.findWithDefault "None" "Name" (property v)
          desc = Map.findWithDefault "None" "Description" (property v) ]
  -- melee
  melee = fromMaybe "None" (Map.lookup "melee" pProp)
  meleeStat = map T.unpack $ T.splitOn ":" $
    fromMaybe "I:1d4:0" (Map.lookup melee descMap)
  (mDam, mWT) = (T.pack (meleeStat!!1), read $ meleeStat!!2) :: (Text, Int)
  -- shoot
  shoot = fromMaybe "None" (Map.lookup "shoot" pProp)
  shootStat = map T.unpack $ T.splitOn ":" $
    fromMaybe "I:1d4:0" (Map.lookup shoot descMap)
  (rDam, rWT) = (T.pack (shootStat!!1), read $ shootStat!!2) :: (Text, Int)
  -- shield
  shield = fromMaybe "None" (Map.lookup "shield" pProp)
  shieldStat = map T.unpack $ T.splitOn ":" $
    fromMaybe "I:0:0" (Map.lookup shield descMap)
  (sAC, sWT) = (read $ shieldStat!!1, read $ shieldStat!!2) :: (Int, Int)
  -- armor
  armor = fromMaybe "None" (Map.lookup "armor" pProp)
  armorStat = map T.unpack $ T.splitOn ":" $
    fromMaybe "I:10:0" (Map.lookup armor descMap)
  (aAC, aWT) = (read $ armorStat!!1, read $ armorStat!!2) :: (Int, Int)
  -- Dex modifier
  pDex = read $ T.unpack $ Map.findWithDefault "1" "dex" pProp :: Int
  pDR n
    | n > 40 = 0
    | n > 20 && n <= 40 = if abilityMod pDex > 2 then 2 else abilityMod pDex
    | otherwise = abilityMod pDex
  -- ArmorClass (AC)
  pAC = aAC + sAC + pDR aWT
  -- Weight (WT)
  pWT = aWT + sWT + mWT + rWT
  -- '@' Combat Stats
  newProp = Map.fromList [ ("AC", T.pack $ show pAC)
                         , ("WT", T.pack $ show pWT)
                         , ("WWT", T.pack $ show mWT)
                         , ("ATTACK", mDam)
                         , ("SHOOT", rDam)
                         ]
  in Map.union newProp pProp

-- | @ Equipment
characterEquipment :: EntityMap -> AssetMap -> [Text]
characterEquipment em _ = let
  (pEntity, _) = getPlayer em
  pProp = property pEntity
  pInv = [melee, shoot, ring, neck, armor, cloak, shield, helmet, hands, feet]
  melee  = T.append "Melee:  " $ fromMaybe "None" (Map.lookup "melee" pProp)
  shoot  = T.append "Shoot:  " $ fromMaybe "None" (Map.lookup "shoot" pProp)
  ring   = T.append "Ring:   " $ fromMaybe "None" (Map.lookup "jewelry" pProp)
  neck   = T.append "Neck:   " $ fromMaybe "None" (Map.lookup "neck" pProp)
  armor  = T.append "Armor:  " $ fromMaybe "None" (Map.lookup "armor" pProp)
  cloak  = T.append "Cloak:  " $ fromMaybe "None" (Map.lookup "cloak" pProp)
  shield = T.append "Shield: " $ fromMaybe "None" (Map.lookup "shield" pProp)
  helmet = T.append "Head:   " $ fromMaybe "None" (Map.lookup "head" pProp)
  hands  = T.append "Hands:  " $ fromMaybe "None" (Map.lookup "hands" pProp)
  feet   = T.append "Feet:   " $ fromMaybe "None" (Map.lookup "feet" pProp)
  ac     = T.append "AC:     " $ fromMaybe "0" (Map.lookup "AC" pProp)
  attack = T.append "Attack: " $ fromMaybe "0" (Map.lookup "ATTACK" pProp)
  range  = T.append "Shoot:  " $ fromMaybe "0" (Map.lookup "SHOOT" pProp)
  -- Encumbered, Finesse, Heavy weapons?
  pStr = read $ T.unpack $ Map.findWithDefault "1" "str" pProp :: Int
  pWT  = read $ T.unpack $ Map.findWithDefault "0" "WT"  pProp :: Int
  pWWT = read $ T.unpack $ Map.findWithDefault "0" "WWT" pProp :: Int
  pEnc = if pWT > 5 * pStr
    then "Load: ENCUMBERED"
    else T.pack $ "Load: " ++ show pWT ++ "/" ++ show (5 * pStr) ++ " lbs."
  pFinesse = if pWWT < 3 then "Melee: Finesse" else "Melee: Strength"
  pHeavy   = if pWWT > 4
    then "Weapon: Heavy"
    else T.pack $ "Weapon: " ++ show pWWT ++ " lbs."
  in selection pInv
  ++ [ac, attack, range, pFinesse, pEnc, pHeavy
     , "Press [0-9] to Doff. (I)nventory. Press ESC to Continue."]

-- | @ Examine
characterExamine :: [Coord] -> EntityMap -> AssetMap -> [Text]
characterExamine fov em _ = let
  view = GE.fromEntityBy em
  pFOV = [ name | (ek, _) <- filter (\(_, j) -> j `elem` fov) view,
           let name = Map.findWithDefault "None" "Name" (property ek) ]
  in selection pFOV
  ++ [" ", "Press [0-9, A-J] to E(X)amine. ESC to Continue."]

-- | @ Inventory
characterInventory :: EntityMap -> AssetMap -> [Text]
characterInventory em _ = let
  (pEntity, _) = getPlayer em
  pItems = filter (\(i, j) -> j > 0 &&
                  i `notElem` ["Arrow", "Coin", "Mushroom", "Potion"]) $
           Map.toList (inventory pEntity)
  pInv = filter (/="None") $
    [ name | (k, v) <- pItems,
      let name = if v > 0
            then T.append k (T.pack $ " (" ++ show v ++ ")")
            else "None" ]
  in selection pInv
  ++ [" ", "Press [0-9, A-J] to Don. (D)rop. (W)ield. ESC to Continue."]

-- | @ Look
-- What can @ see in FOV...
characterLook :: [Coord] -> EntityMap -> [Text]
characterLook fov em = let
  groupF :: [Text] -> [(Text, Int)]
  groupF = map (head &&& length) . group . sort
  view = GE.fromEntityBy em
  entities = groupF $
    [ name | (ek, _) <- filter (\(_, j) -> j `elem` fov) view,
      let label = Map.findWithDefault "None" "Name" (property ek)
          mHP = eHP ek
          mMaxHP = eMaxHP ek
          status = condition label mHP mMaxHP
          name = if mHP > 0
                 then status
                 else label ]
  pFOV = [ seen | (i, j) <- entities,
           let seen = if j > 1
                 then T.append i $ T.pack $ " <" ++ show j ++ ">"
                 else i ]
  in selection pFOV

-- | @ Stats
characterSheet :: EntityMap -> [Text]
characterSheet em = let
  (pEntity, _) = getPlayer em
  pInv   = inventory pEntity
  pCoin  = T.append "AU: " (T.pack $ show $ Map.findWithDefault 0 "Coin" pInv)
  pProp = property pEntity
  pEquip = T.concat [ equip "melee" "|" pProp
    , equip "shoot"   "{" pProp
    , equip "jewelry" "=" pProp
    , equip "neck"    "\"" pProp
    , equip "armor"   "[" pProp
    , equip "cloak"   "(" pProp
    , equip "shield"  ")" pProp
    , equip "head"    "]" pProp
    , equip "hands"   "]" pProp
    , equip "feet"    "]" pProp
    ]
  pCls  = Map.findWithDefault "Player" "Class" pProp
  pStr  = T.append "Str: " (Map.findWithDefault "1" "str" pProp)
  pDex  = T.append "Dex: " (Map.findWithDefault "1" "dex" pProp)
  pCon  = T.append "Con: " (Map.findWithDefault "1" "con" pProp)
  pInt  = T.append "Int: " (Map.findWithDefault "1" "int" pProp)
  pWis  = T.append "Wis: " (Map.findWithDefault "1" "wis" pProp)
  pLvl  = T.pack $ "Level: " ++ show (eLvl pEntity)
  pExp  = T.pack $ "EXP: " ++ show (eXP pEntity)
  in [ pCls, pLvl, pExp, pCoin, pEquip, pStr, pDex, pCon, pInt, pWis ]

-- | @ Store
characterStore :: EntityMap -> AssetMap -> [Text]
characterStore em am = let
  (pEntity, _) = getPlayer em
  descMap = Map.fromList $
    [ (name, desc) | (_, v) <- Map.toList am,
      let name = Map.findWithDefault "None" "Name" (property v)
          desc = Map.findWithDefault "None" "Description" (property v) ]
  pInv = filter (/="None") $
    [ name | (k, v) <- Map.toList (inventory pEntity),
      let name = if k `elem` ["Arrow", "Mushroom", "Potion"]
            then T.append k (T.pack $ " (" ++ show v ++ ") " ++ T.unpack item)
            else "None"
          item = Map.findWithDefault "--" k descMap ]
  in selection pInv
  ++ [" ", "Press [0-9, A-J] to Purchase. ESC to Continue."]

-- | P condition
-- Green, Red, Purple...
condition :: Text -> Int -> Int -> Text
condition label hp maxHP = let
  status n
    | n < 5 = "*"
    | (maxHP `div` n) > 2 = "!"
    | otherwise = ":"
  entry = T.append label $
    T.pack $ " (HP" ++ status hp ++ " " ++ show hp ++ "/" ++ show maxHP ++ ")"
  in entry

-- | @ equipment
equip :: Text -> Text -> Properties -> Text
equip name desc prop = let
  item = fromMaybe "None" (Map.lookup name prop)
  equipped n
    | n == "None" = "."
    | otherwise = desc
  in equipped item

-- | @ lives at 0
-- Arrow for Player
getArrow :: EntityMap -> Double
getArrow em = let
  (pEntity, _) = getPlayer em
  pArrow = fromIntegral $ Map.findWithDefault 0 "Arrow" (inventory pEntity)
  in pArrow / 20.0

-- | @ lives at 0
-- Health for Player
getHealth :: EntityMap -> Double
getHealth em = let
  (pEntity, _) = getPlayer em
  hp    = fromIntegral $ eHP pEntity
  maxHp = fromIntegral $ eMaxHP pEntity
  in hp / maxHp

-- | @ lives at 0
-- Mushroom for Player
getMushroom :: EntityMap -> Double
getMushroom em = let
  (pEntity, _) = getPlayer em
  pMush = fromIntegral $ Map.findWithDefault 0 "Mushroom" (inventory pEntity)
  in pMush / 20.0

-- | @ lives at 0
-- Mana for Player
getMana :: EntityMap -> Double
getMana em = let
  (pEntity, _) = getPlayer em
  mp    = fromIntegral $ eMP pEntity
  maxMp = fromIntegral $ eMaxMP pEntity
  in mp / maxMp

-- | @ lives at 0
-- getPlayer
getPlayer :: EntityMap -> (Player, Coord)
getPlayer = GE.getEntityAt 0

-- | @ lives at 0
-- Potion for Player
getPotion :: EntityMap -> Double
getPotion em = let
  (pEntity, _) = getPlayer em
  pPot = fromIntegral $ Map.findWithDefault 0 "Potion" (inventory pEntity)
  in pPot / 20.0

-- | @ gets better with level
proficiency :: Int -> Int
proficiency lvl
  | lvl >= 1  && lvl <= 4 = 2
  | lvl >= 5  && lvl <= 8 = 3
  | lvl >= 9  && lvl <= 12 = 4
  | lvl >= 13 && lvl <= 16 = 5
  | lvl >= 17 && lvl <= 20 = 6
  | otherwise = 2

-- | @ selection
selection :: [Text] -> [Text]
selection xs = let
  pSel = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
        , "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
  pInv = [ name | (k, v) <- zip pSel xs, let name = T.concat [k, ") ", v] ]
  in pInv

-- | update @ properties
updatePlayer :: Player -> EntityMap -> EntityMap
updatePlayer = Map.insert 0

-- | update @ position
updatePlayerBy :: Coord -> EntityMap -> EntityMap
updatePlayerBy = GE.updateEntityPos 0

-- | updateEntityXP at ix
updatePlayerXP :: Int -> EntityMap -> EntityMap
updatePlayerXP xp em = let
  (pEntity, _ ) = getPlayer em
  pProp     = property pEntity
  pCls      = Map.findWithDefault "None" "Class" pProp
  pStr      = read $ T.unpack $ Map.findWithDefault "1" "str" pProp
  pDex      = read $ T.unpack $ Map.findWithDefault "1" "dex" pProp
  pCon      = read $ T.unpack $ Map.findWithDefault "1" "con" pProp
  pInt      = read $ T.unpack $ Map.findWithDefault "1" "int" pProp
  pWis      = read $ T.unpack $ Map.findWithDefault "1" "wis" pProp
  cHP       = read $ T.unpack $ Map.findWithDefault "1" "HP" pProp
  cMP       = read $ T.unpack $ Map.findWithDefault "1" "MP" pProp
  pTot      = eXP pEntity + xp
  pLvl      = xpLevel pTot
  current   = eLvl pEntity
  pHP       = if pLvl > current then pMaxHP else eHP pEntity
  pMaxHP    = pLvl * (cHP + abilityMod pCon)
  -- Fighter
  (fStr, fDex) = if pCls == "Fighter" then abilityGain pLvl current else (0,0)
  -- Rogue
  (rDex, rStr) = if pCls == "Rogue" then abilityGain pLvl current else (0,0)
  -- Mage
  (mInt, mWis) = if pCls == "Mage"  then abilityGain pLvl current else (0,0)
  -- Cleric
  (cWis, cStr) = if pCls == "Cleric" then abilityGain pLvl current else (0,0)
  -- Mana
  pMP     = if pLvl > current then pMaxMP else eMP pEntity
  newWis  = pWis + mWis + cWis
  pMaxMP  = pLvl * (cMP + abilityMod newWis)
  -- Properties
  newProp = Map.fromList [ ("str", T.pack $ show $ pStr + fStr + rStr + cStr)
                         , ("dex", T.pack $ show $ pDex + fDex + rDex)
                         , ("int", T.pack $ show $ pInt + mInt)
                         , ("wis", T.pack $ show $ pWis + mWis + cWis)
                         , ("Proficiency", T.pack $ show $ proficiency pLvl) ]
  newPlayer = pEntity { property = Map.union newProp pProp
                      , eLvl     = pLvl
                      , eHP      = pHP
                      , eMaxHP   = pMaxHP
                      , eMP      = pMP
                      , eMaxMP   = pMaxMP
                      , eXP      = pTot }
  in updatePlayer newPlayer em

-- | xpLevel simple
xpLevel :: Int -> Int
xpLevel x
  | x > 0    && x <= 35  = 1
  | x > 35   && x <= 100 = 2
  | x > 100  && x <= 200 = 3
  | x > 200  && x <= 300 = 4
  | x > 300  && x <= 400 = 5
  | x > 400  && x <= 600 = 6
  | x > 600  && x <= 800 = 7
  | x > 800  && x <= 1000 = 8
  | x > 1000 && x <= 1200 = 9
  | x > 1200 && x <= 1400 = 10
  | x > 1400 && x <= 1600 = 11
  | x > 1600 && x <= 2000 = 12
  | x > 2000 && x <= 3000 = 13
  | x > 3000 && x <= 4600 = 14
  | x > 4000 && x <= 5000 = 15
  | x > 5000 && x <= 6000 = 16
  | x > 6000 && x <= 7000 = 17
  | x > 7000 && x <= 8000 = 18
  | x > 8000 && x <= 10000 = 19
  | x > 10000              = 20
  | otherwise = 1
