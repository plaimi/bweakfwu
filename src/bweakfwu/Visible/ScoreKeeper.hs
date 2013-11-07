{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
-
- This file is part of bweakfwu
-
- bweakfwu is free software: you can redistribute it and/or modify
- it under the terms of the GNU General Public License as published by
- the Free Software Foundation, either version 3 of the License, or
- (at your option) any later version.
-
- bweakfwu is distributed in the hope that it will be useful,
- but WITHOUT ANY WARRANTY; without even the implied warranty of
- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
- GNU General Public License for more details.
-
- You should have received a copy of the GNU General Public License
- along with bwekfwu  If not, see <http://www.gnu.org/licenses/>.
-} module Visible.ScoreKeeper where

import Graphics.Gloss.Data.Color (dark, cyan)
import Graphics.Gloss.Data.Picture (Picture (Color, Scale, Text, Translate))

import Visible (Visible, render)

data ScoreKeeper =
  -- | A 'ScoreKeeper' consists of two 'Score's.
  ScoreKeeper Score Score

type Score = Float

instance Visible ScoreKeeper where
  -- | 'render' draws a 'ScoreKeeper' at the top of the 'World'.
  render s =
    Color (dark cyan)
    $ Translate (-5.0) 18.0
    $ Scale 0.03 0.03
    $ Text (stringScore s)

stringScore ::  ScoreKeeper -> String
-- | 'stringScore' makes a string representation of the 'Score's of a
-- 'ScoreKeeper'.
stringScore (ScoreKeeper s1 s2) =
  show (realScore s1) ++ " : " ++ show (realScore s2)

realScore ::  Score -> Int
-- | 'realScore' gets the effective score of a 'Score'. The 'Score's of
-- a 'ScoreKeeper' are floating point numbers for the sake of adding and
-- deducting points, but the effective score of a player is an integer.
realScore s = floor s :: Int

mergeScores ::  [ScoreKeeper] -> ScoreKeeper
-- | 'mergeScores' takes a list of 'ScoreKeeper's and merges them into one
-- 'ScoreKeeper'.
mergeScores sk =
  ScoreKeeper s1 s2
  where s1 = sum (map (fst . scores) sk)
        s2 = sum (map (snd . scores) sk)

scores ::  ScoreKeeper -> (Score, Score)
-- | 'scores' returns the 'Score's of a 'ScoreKeeper' as a tuple.
scores (ScoreKeeper s1 s2) = (s1, s2)
