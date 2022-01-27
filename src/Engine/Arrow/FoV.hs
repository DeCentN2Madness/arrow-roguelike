{-

Engine.Arrow.FoV.hs

Precise Permissive Field of View
<http://www.roguebasin.com/index.php/Precise_Permissive_Field_of_View>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.FoV (checkFov) where

import Prelude hiding (pred)
import Data.Set (Set)
import qualified Data.Set as S

data SightLine = SightLine Int Int Int Int deriving (Eq, Show)
data ViewBump = ViewBump Int Int (Maybe ViewBump) deriving (Eq, Show)

data View = View {
  getShallowBumps :: [(Int, Int)]
  , getShallowLine :: SightLine
  , getSteepBumps :: [(Int, Int)]
  , getSteepLine :: SightLine
  } deriving (Eq, Show)

type  VisionBlocked = (Int, Int) -> Bool

-- | abovePoint infix
abovePoint :: SightLine -> (Int, Int) -> Bool
abovePoint self p = relativeSlope self p < 0

-- | aboveOrCollinearPoint infix
aboveOrCollinearPoint :: SightLine -> (Int, Int) -> Bool
aboveOrCollinearPoint self p = relativeSlope self p <= 0

-- | addShallowBump
addShallowBump :: (Int, Int) -> View -> View
addShallowBump loc@(x, y) view = let
  (SightLine xi yi _ _) = getShallowLine view
  newShallowBumps = loc : getShallowBumps view
  newShallowLine = foldl (foldLineUsing abovePoint) (SightLine xi yi x y) (getSteepBumps view)
  in view { getShallowBumps = newShallowBumps, getShallowLine = newShallowLine }

-- | addSteepBump
addSteepBump :: (Int, Int) -> View -> View
addSteepBump loc@(x, y) view = let
  (SightLine xi yi _ _) = getSteepLine view
  newSteepBumps = loc : getSteepBumps view
  newSteepLine = foldl (foldLineUsing belowPoint) (SightLine xi yi x y) (getShallowBumps view)
  in view { getSteepBumps = newSteepBumps, getSteepLine = newSteepLine }

-- | belowOrCollinearPoint infix
belowOrCollinearPoint :: SightLine -> (Int, Int) -> Bool
belowOrCollinearPoint self p = relativeSlope self p >= 0

-- | belowPoint infix
belowPoint :: SightLine -> (Int, Int) -> Bool
belowPoint self p = relativeSlope self p > 0

-- | bumpAndCheck checks and returns the new view list in single op
bumpAndCheck :: ((Int, Int) -> View -> View) -> [View] -> Int -> (Int, Int) -> [View]
bumpAndCheck bumpf activeViews viewIndex bump = out
  where
    view = activeViews !! viewIndex
    bumpedView = bumpf bump view
    out = if validView bumpedView
      then update viewIndex activeViews bumpedView
      else remove viewIndex activeViews

-- | calcViewIndex is a version of `dropWhile`
calcViewIndex :: [View] -> (Int, Int) -> Int
calcViewIndex activeViews bottomRight = let
  go tmp [] = tmp
  go tmp (v:vs) = if getSteepLine v `belowOrCollinearPoint` bottomRight
    then go (tmp+1) vs
    else tmp
  in go 0 activeViews

-- | Perform view calculations on a single Quandrant relative to
-- the start position of the overall FOV computation.
checkQuadrant :: VisionBlocked -> Int -> (Int, Int) -> (Int, Int) -> Set (Int, Int)
checkQuadrant vision range (sx, sy) (qx, qy) = checkSub coordsToCheck (S.singleton (sx, sy)) startViewList
  where
    shallowLineStart = SightLine 0 1 range 0
    steepLineStart   = SightLine 1 0 0     range
    startViewList    = [mkView shallowLineStart steepLineStart]
    coordsToCheck    = coordsFromRange range
    checkSub :: [(Int, Int)] -> Set (Int, Int) -> [View] -> Set (Int, Int)
    checkSub _  visited [] = visited
    checkSub [] visited _ = visited
    checkSub ((dx, dy):cs) visited activeViews = checkSub cs newVisited newActiveViews
      where (newVisited, newActiveViews) = visitCoord (sx, sy) (dx, dy) (qx, qy) activeViews vision visited

-- | checkFov to check the Precise Permissive Field of View
-- for @origin@ vs @xs@ coordinates at @distance@.
--
-- Summary: PPFoV
-- .
-- .
-- .
-- 9
-- 5 8
-- 2 4 7
-- @ 1 3 6 ...
checkFov :: (Int, Int) -> [(Int,Int)] -> Int -> Set (Int, Int)
checkFov origin xs distance = let
  blocked = (`elem` xs)
  in fov blocked distance origin

-- | collinearPpoint infix
collinearPoint :: SightLine -> (Int, Int) -> Bool
collinearPoint self p = relativeSlope self p == 0

-- | collinearLine infix
collinearLine :: SightLine -> SightLine -> Bool
collinearLine self (SightLine xi yi xf yf) =
  collinearPoint self (xi, yi) && collinearPoint self (xf, yf)

-- | coordsFromRange the list of coords from start position
coordsFromRange :: Int -> [(Int, Int)]
coordsFromRange range = do
  let maxIndex = (2*range) + 1
  i <- [1..(maxIndex-1)]
  let startJ  = max (i-range) 0
  let maxJ    = min i range + 1
  j <- [startJ .. (maxJ-1)]
  pure (i-j, j)

-- | foldLineUsing for SightLine fn
foldLineUsing :: (SightLine -> (Int, Int) -> Bool) -> SightLine -> (Int, Int) -> SightLine
foldLineUsing pred line@(SightLine _ _ xf yf) loc@(x, y) = if line `pred` loc
  then SightLine x y xf yf
  else line

-- | fov
-- fov checks vision at range from start position
-- vision is fn to check Coord which block vision
-- see checkFov
fov :: VisionBlocked -> Int -> (Int, Int) -> Set (Int, Int)
fov vision range start = unionMap (checkQuadrant vision range start)
  [(1, 1), ((-1), 1), (1, (-1)), ((-1), (-1))]

-- | makes a new view using shallow and steep line w/o bumps
mkView :: SightLine -> SightLine -> View
mkView shallowLine steepLine = View {
  getShallowBumps = []
  , getShallowLine = shallowLine
  , getSteepBumps = []
  , getSteepLine = steepLine
  }

-- | relativeSlope infix
relativeSlope :: SightLine -> (Int, Int) -> Int
relativeSlope (SightLine xi yi xf yf) (x, y) = let
  dx = xf - xi
  dy = yf - yi
  in (dx * (xf -x)) - (dy * (yf-y))

-- | adds a single location to the set of visited locations if its
-- within one of the views, and updates any views as necessary.
visitCoord :: (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> [View]
  -> VisionBlocked
  -> Set (Int, Int)
  -> (Set (Int, Int), [View])
visitCoord (sx, sy) (dx, dy) (qx, qy) activeViews vision visited = let
  topLeft = (dx, dy+1)
  bottomRight = (dx+1, dy)
  realX = dx * qx
  realY = dy * qy
  trueLocation = (sx + realX, sy + realY)
  viewIndex = calcViewIndex activeViews bottomRight
  in if viewIndex == length activeViews || getShallowLine (activeViews !! viewIndex) `aboveOrCollinearPoint` topLeft
  then (visited, activeViews) -- no compatible views
  else let newVisited = S.insert trueLocation visited
           visionBlocked = vision trueLocation
           in if visionBlocked
                 then let currentView = activeViews !! viewIndex -- vision is blocked
                          shallowAboveBottomRight = getShallowLine currentView `abovePoint` bottomRight
                          steepBelowTopLeft = getSteepLine currentView `belowPoint` topLeft
                          in case (shallowAboveBottomRight, steepBelowTopLeft) of
                               (True, True) -> (newVisited, remove viewIndex activeViews)
                               (True, False) -> (newVisited, bumpAndCheck addShallowBump activeViews viewIndex topLeft)
                               (False, True) -> (newVisited, bumpAndCheck addSteepBump activeViews viewIndex bottomRight)
                               (False, False) -> do
                                 let clonedViews = add viewIndex activeViews currentView
                                 let shallowChecked = bumpAndCheck addShallowBump clonedViews (viewIndex+1) topLeft
                                 let steepChecked = bumpAndCheck addSteepBump shallowChecked viewIndex bottomRight
                                     in (newVisited, steepChecked)
                 else (newVisited, activeViews) -- vision not blocked

-- | unionMap maps fn over list and returns Set
unionMap :: (Ord b) => (a -> Set b) -> [a] -> Set b
unionMap f list = S.unions $ map f list

-- | validView `infix`
validView :: View -> Bool
validView view = not (shallowIsSteep && lineOnExtremity)
  where
    shallowIsSteep = shallowLine' `collinearLine` steepLine'
    lineOnExtremity = shallowLine' `collinearPoint` (0, 1) || shallowLine' `collinearPoint` (1, 0)
    shallowLine' = getShallowLine view
    steepLine' = getSteepLine view

add :: (Integral a) => a -> [b] -> b -> [b]
add a (b:bs) new
  | a < 0 = b : bs
  | a == 0 = new : b : bs
  | otherwise = b : add (a-1) bs new
add 0 [] new = [new]
add _ [] _   = []

update :: (Integral a) => a -> [b] -> b -> [b]
update a (b:bs) new
  | a < 0 = b : bs
  | a == 0 = new : bs
  | otherwise = b : update (a-1) bs new
update _ [] _ = []

remove :: (Integral a) => a -> [b] -> [b]
remove a (b:bs)
  | a < 0 = b : bs
  | a == 0 = bs
  | otherwise = b : remove (a-1) bs
remove _ [] = []
