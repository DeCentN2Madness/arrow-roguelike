{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Beasts.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Kind.Beasts where

import Data.Text (Text)

type Prop = [(Text, Text)]

-- | fighter
fighter :: Prop
fighter = [ ("str", "15")
          , ("dex", "14")
          , ("con", "13")
          , ("int", "12")
          , ("wis", "10")
          , ("HP", "10")
          , ("MP", "0")
          , ("XP", "0")
          , ("Proficiency", "2")
          , ("Class", "Fighter")
          , ("AC", "11")
          , ("WT", "11")
          , ("WWT", "1")
          , ("ATTACK", "1d4")
          , ("SHOOT", "1d4")
          , ("ATTACKS", "1")
          , ("CAST", "0")
          , ("melee", "melee/Dagger")
          , ("shoot", "shoot/Sling")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Leather")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Mouse
smBeast :: Prop
smBeast = [ ("str", "7")
          , ("dex", "15")
          , ("con", "11")
          , ("int", "2")
          , ("wis", "10")
          , ("HP", "7")
          , ("MP", "0")
          , ("XP", "25")
          , ("Proficiency", "1")
          , ("Class", "Beast")
          , ("AC", "12")
          , ("WT", "0")
          , ("WWT", "0")
          , ("ATTACK", "1d4")
          , ("SHOOT", "1d1")
          , ("ATTACKS", "1")
          , ("CAST", "0")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Squeak")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "None")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Wolf
mdBeast :: Prop
mdBeast = [ ("str", "12")
          , ("dex", "15")
          , ("con", "12")
          , ("int", "3")
          , ("wis", "12")
          , ("HP", "11")
          , ("MP", "0")
          , ("XP", "50")
          , ("Proficiency", "2")
          , ("Class", "Beast")
          , ("AC", "13")
          , ("WT", "0")
          , ("WWT", "0")
          , ("ATTACK", "2d4")
          , ("SHOOT", "1d2")
          , ("ATTACKS", "1")
          , ("CAST", "0")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Howl")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Natural Armor")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Dire Wolf
lgBeast :: Prop
lgBeast = [ ("str", "17")
          , ("dex", "15")
          , ("con", "15")
          , ("int", "3")
          , ("wis", "12")
          , ("HP", "37")
          , ("MP", "0")
          , ("XP", "200")
          , ("Proficiency", "2")
          , ("Class", "Beast")
          , ("AC", "14")
          , ("WT", "0")
          , ("WWT", "10")
          , ("ATTACK", "2d5")
          , ("SHOOT",  "1d2")
          , ("ATTACKS", "1")
          , ("CAST", "0")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Howl")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Natural Armor")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Dragon Wyrmling
mdDragon :: Prop
mdDragon = [ ("str", "15")
           , ("dex", "12")
           , ("con", "13")
           , ("int", "14")
           , ("wis", "11")
           , ("HP", "38")
           , ("MP", "0")
           , ("XP", "450")
           , ("Proficiency", "2")
           , ("Class", "Dragon")
           , ("AC", "17")
           , ("WT", "0")
           , ("WWT", "10")
           , ("ATTACK", "1d10")
           , ("SHOOT", "1d8")
           , ("ATTACKS", "2")
           , ("CAST", "0")
           , ("Throw", "breathes!")
           , ("melee", "melee/Bite")
           , ("shoot", "shoot/Breath")
           , ("jewelry", "None")
           , ("neck", "None")
           , ("armor", "armor/Natural Armor")
           , ("cloak", "None")
           , ("shield", "None")
           , ("head", "None")
           , ("hands", "None")
           , ("feet", "None")
           ]

-- | Wyvern
lgDragon :: Prop
lgDragon = [ ("str", "19")
           , ("dex", "10")
           , ("con", "16")
           , ("int", "5")
           , ("wis", "12")
           , ("HP", "110")
           , ("MP", "0")
           , ("XP", "2300")
           , ("Proficiency", "3")
           , ("Class", "Dragon")
           , ("AC", "13")
           , ("WT", "0")
           , ("WWT", "10")
           , ("ATTACK", "2d8")
           , ("SHOOT", "2d6")
           , ("ATTACKS", "3")
           , ("CAST", "0")
           , ("Throw", "breathes!")
           , ("melee", "melee/Bite")
           , ("shoot", "shoot/Sting")
           , ("jewelry", "None")
           , ("neck", "None")
           , ("armor", "armor/Natural Armor")
           , ("cloak", "None")
           , ("shield", "None")
           , ("head", "None")
           , ("hands", "None")
           , ("feet", "None")
           ]

-- | Hydra
lgMonster :: Prop
lgMonster = [ ("str", "20")
           , ("dex", "12")
           , ("con", "20")
           , ("int", "2")
           , ("wis", "10")
           , ("HP", "172")
           , ("MP", "0")
           , ("XP", "3900")
           , ("Proficiency", "3")
           , ("Class", "Monster")
           , ("AC", "15")
           , ("WT", "0")
           , ("WWT", "10")
           , ("ATTACK", "1d10")
           , ("SHOOT", "0")
           , ("ATTACKS", "3")
           , ("CAST", "0")
           , ("Throw", "breathes!")
           , ("melee", "melee/Bite")
           , ("shoot", "None")
           , ("jewelry", "None")
           , ("neck", "None")
           , ("armor", "armor/Natural Armor")
           , ("cloak", "None")
           , ("shield", "None")
           , ("head", "None")
           , ("hands", "None")
           , ("feet", "None")
           ]

-- | Spider
lgSpider :: Prop
lgSpider = [ ("str", "14")
          , ("dex", "16")
          , ("con", "12")
          , ("int", "2")
          , ("wis", "11")
          , ("HP", "26")
          , ("MP", "0")
          , ("XP", "200")
          , ("Proficiency", "2")
          , ("Class", "Beast")
          , ("AC", "14")
          , ("WT", "0")
          , ("WWT", "0")
          , ("ATTACK", "1d8")
          , ("SHOOT",  "1d6")
          , ("ATTACKS", "1")
          , ("CAST", "0")
          , ("Throw", "spits!")
          , ("melee", "melee/Bite")
          , ("shoot", "shoot/Web")
          , ("jewelry", "None")
          , ("neck", "None")
          , ("armor", "armor/Natural Armor")
          , ("cloak", "None")
          , ("shield", "None")
          , ("head", "None")
          , ("hands", "None")
          , ("feet", "None")
          ]

-- | Troll
gtHumanoid :: Prop
gtHumanoid = [ ("str", "18")
             , ("dex", "13")
             , ("con", "20")
             , ("int", "7")
             , ("wis", "9")
             , ("HP", "84")
             , ("MP", "0")
             , ("XP", "1800")
             , ("Proficiency", "2")
             , ("Class", "Giant")
             , ("AC", "15")
             , ("WT", "21")
             , ("WWT", "10")
             , ("ATTACK", "2d6")
             , ("SHOOT", "1d8")
             , ("ATTACKS", "2")
             , ("CAST", "0")
             , ("Throw", "burps!")
             , ("melee", "melee/Claw")
             , ("shoot", "shoot/Spit")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Natural Armor")
             , ("cloak", "None")
             , ("shield", "None")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
             ]

-- | Ogre
lgHumanoid :: Prop
lgHumanoid = [ ("str", "19")
             , ("dex", "8")
             , ("con", "16")
             , ("int", "5")
             , ("wis", "7")
             , ("HP", "59")
             , ("MP", "0")
             , ("XP", "450")
             , ("Proficiency", "2")
             , ("Class", "Giant")
             , ("AC", "13")
             , ("WT", "24")
             , ("WWT", "10")
             , ("ATTACK", "2d8")
             , ("SHOOT", "1d6")
             , ("ATTACKS", "1")
             , ("CAST", "0")
             , ("Throw", "hurls!")
             , ("melee", "melee/Greatclub")
             , ("shoot", "shoot/Javelin")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Hide")
             , ("cloak", "None")
             , ("shield", "None")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
             ]

-- | Orc
mdHumanoid :: Prop
mdHumanoid = [ ("str", "16")
             , ("dex", "12")
             , ("con", "16")
             , ("int", "7")
             , ("wis", "11")
             , ("HP", "15")
             , ("MP", "0")
             , ("XP", "100")
             , ("Proficiency", "2")
             , ("Class", "Humanoid")
             , ("AC", "13")
             , ("WT", "21")
             , ("WWT", "7")
             , ("ATTACK", "1d12")
             , ("SHOOT", "1d6")
             , ("ATTACKS", "1")
             , ("CAST", "0")
             , ("Throw", "throws!")
             , ("melee", "melee/Greataxe")
             , ("shoot", "shoot/Javelin")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Hide")
             , ("cloak", "None")
             , ("shield", "None")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
             ]

-- | Orc Archer
mdHumanoidA :: Prop
mdHumanoidA = mdHumanoid
  ++ [ ("ATTACK", "1d4")
     , ("AC", "12")
     , ("WT", "14")
     , ("WWT", "1")
     , ("melee", "melee/Dagger")
     , ("armor", "armor/Leather")
     ]

-- | Orc Shaman
mdHumanoidM :: Prop
mdHumanoidM = mdHumanoid
  ++ [ ("int", "11")
     , ("ATTACK", "1d6")
     , ("SHOOT", "1d4")
     , ("CAST", "2d4")
     , ("AC", "11")
     , ("WT", "5")
     , ("WWT", "4")
     , ("Throw", "curses!")
     , ("melee", "melee/Quarterstaff")
     , ("shoot", "shoot/Dart")
     , ("armor", "None")
     , ("WT", "18")
     , ("WWT", "4")
     , ("MP", "10")
     ]

-- | Goblin
smHumanoid :: Prop
smHumanoid = [ ("str", "8")
             , ("dex", "14")
             , ("con", "10")
             , ("int", "10")
             , ("wis", "8")
             , ("HP", "7")
             , ("MP", "0")
             , ("XP", "50")
             , ("Proficiency", "2")
             , ("Class", "Humanoid")
             , ("AC", "15")
             , ("WT", "20")
             , ("WWT", "2")
             , ("ATTACK", "1d6")
             , ("SHOOT", "1d6")
             , ("ATTACKS", "1")
             , ("CAST", "0")
             , ("Throw", "shoots!")
             , ("melee", "melee/Scimitar")
             , ("shoot", "shoot/Shortbow")
             , ("jewelry", "None")
             , ("neck", "None")
             , ("armor", "armor/Leather")
             , ("cloak", "None")
             , ("shield", "shield/Shield")
             , ("head", "None")
             , ("hands", "None")
             , ("feet", "None")
             ]
