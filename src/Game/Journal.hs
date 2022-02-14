{-# LANGUAGE OverloadedStrings #-}
{-

Game.Journal.hs

Game.Journal is round-robin Text returns from the Map w/ [(ix, Text)]

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Journal (fromJournal
               , mkTextMap
               , TextMap
               , updateJournal) where

import Prelude hiding (lookup)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

type TextMap = Map Int Text

-- | fromJournal reads TextMap at ix
fromJournal :: [Int] -> TextMap -> [(Int, Text)]
fromJournal [] _ = []
fromJournal (x:xs) tm = let
  t = getJournalAt x tm
  in (x, t) : fromJournal xs tm

-- | getJournalAt
getJournalAt :: Int -> TextMap -> Text
getJournalAt ix tm = let
  t = fromMaybe (T.pack "...") $ Map.lookup ix tm
  in t

-- | mkTextMap starts the TextMap
mkTextMap :: TextMap
mkTextMap = Map.empty

-- | updateJournal
-- keep 20 entries in a round-robin
updateJournal :: [Text] -> TextMap -> TextMap
updateJournal xs tm = let
  entry = zip [0..] $ filter (/="...") xs
  newJournal = take 20 $ [(k, t) | (i, t) <- Map.toList tm,
                          let k = i + length entry]
  in Map.fromList (entry ++ newJournal)
