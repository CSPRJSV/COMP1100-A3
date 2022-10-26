{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : AI
Description : AIs for Conseuctive Dots
Copyright   : (c) 2022 ANU
License     : AllRightsReserved
-}
module AI where

import           ConsecutiveDots



-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- their attention to marking. DO NOT submit with firstColumn as "default".
ais :: [(String, AIFunc)]
ais = [("default", NoLookahead firstColumn)]


-- | A very simple AI, which tries moving in each column until
-- it finds a legal move.
firstColumn :: GameState -> Move
firstColumn state = try 0
  where
    try n = case applyMove n state of
      Nothing -> try (n + 1)
      Just _ -> n