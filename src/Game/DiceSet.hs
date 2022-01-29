{-

Game.DiceSet.hs

Game.DiceSet rolls dice

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.DiceSet (roll3D6
                     , bigRoll
                     , d4
                     , d6
                     , d8
                     , d10
                     , d12
                     , d20
                     , d100
                     , randomList) where

import Control.Monad.Random
import Control.Monad.Trans.State

type Dice = (Int, Int)

-- | bag of dice
d4 :: StdGen -> Int
d4 g = sum $ randomList 1 (1, 4) g

d6 :: StdGen -> Int
d6 g = sum $ randomList 1 (1, 6) g

d8 :: StdGen -> Int
d8 g = sum $ randomList 1 (1, 8) g

d10 :: StdGen -> Int
d10 g = sum $ randomList 1 (1, 10) g

d12 :: StdGen -> Int
d12 g = sum $ randomList 1 (1, 12) g

d20 :: StdGen -> Int
d20 g = sum $ randomList 1 (1, 20) g

d100 :: StdGen -> Int
d100 g = sum $ randomList 1 (1, 100) g

-- example
-- rolls all the same, fixme
threeD6 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
threeD6 i = do
  r0 <- getRandomR (1,6)
  r1 <- getRandomR (1,6)
  r2 <- getRandomR (1,6)
  s <- lift get
  lift $ put ([], i + r0 + r1 + r2 + snd s)

-- | roll3D6 with mods
roll3D6 :: RandomGen g => Int -> g -> Int
roll3D6 i g = let
  r0 = snd $ flip execState ([], i) $ evalRandT (threeD6 i) g
  in r0

-- | bigRoll to create different values
bigRoll :: RandomGen g => Int -> g -> Int
bigRoll i g = let
  randList = randomList (i*10) (4, 6) g
  in randList!!i

-- | randomList of rolls
-- example: roll 6 3d6
--          or roll 10 2d10
--          or roll 1000 4d6
randomList :: (RandomGen g) => Int -> Dice -> g -> [Int]
randomList 0 (_, _) _ = []
randomList n (rolls, sides) gen = let
  (r, newGen) = randomR (rolls, rolls * sides) gen
  in r : randomList (n-1) (rolls, sides) newGen
