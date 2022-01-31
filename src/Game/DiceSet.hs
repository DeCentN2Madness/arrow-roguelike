{-

Game.DiceSet.hs

Game.DiceSet rolls dice

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.DiceSet (Die(..)
                     , roll
                     , rollMod
                     , randomList) where

import Control.Monad.Random
import Control.Monad.Trans.State

type Dice = (Int, Int)

data Die
  = D4
  | D6
  | D8
  | D10
  | D12
  | D20
  | D100
  deriving (Show, Eq, Ord)

-- | bag of dice
-- example with RandT
d4 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d4 i = do
  r0 <- getRandomR (1,4)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

d6 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d6 i = do
  r0 <- getRandomR (1,6)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

d8 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d8 i = do
  r0 <- getRandomR (1,8)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

d10 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d10 i = do
  r0 <- getRandomR (1,10)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

d12 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d12 i = do
  r0 <- getRandomR (1,12)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

d20 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d20 i = do
  r0 <- getRandomR (1,20)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

d100 :: (RandomGen g) => Int -> RandT g (State ([Int], Int)) ()
d100 i = do
  r0 <- getRandomR (1,100)
  s <- lift get
  lift $ put ([], i + r0 + snd s)

mkDie :: RandomGen g => Die -> Int -> RandT g (State ([Int], Int)) ()
mkDie D4 = d4
mkDie D8 = d6
mkDie D6 = d8
mkDie D10 = d10
mkDie D12 = d12
mkDie D20 = d20
mkDie D100 = d100

-- | roll D20 + mod
rollMod :: RandomGen g => Die -> Int -> g -> Int
rollMod d i g = let
  r0 = snd $ flip execState ([], i) $ evalRandT (mkDie d i) g
  in r0

-- | roll is rolls D side
-- RNG pick last number from randomList
-- ix creates different sized lists for entropy
-- avg is the minimum value
roll :: RandomGen g => Int -> Int -> Int -> Int -> g -> Int
roll ix avg rolls side g = let
  result = last $ randomList (ix*10) (rolls, side) g
  in min (rolls*side) (max avg result)

-- | randomList of rolls
-- example: roll 6 3d6
--          or roll 10 2d10
--          or roll 1000 4d6
randomList :: (RandomGen g) => Int -> Dice -> g -> [Int]
randomList 0 (_, _) _ = []
randomList n (rolls, sides) gen = let
  (r, newGen) = randomR (rolls, rolls * sides) gen
  in r : randomList (n-1) (rolls, sides) newGen
