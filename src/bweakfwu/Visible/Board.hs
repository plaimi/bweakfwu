{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
-
- This file is part of bweakfwu.
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
-} module Visible.Board where

import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Data.Picture (Picture (Pictures))

import Visible (Visible, render)
import Visible.Brick (Brick (Brick))

data Board =
  -- | A 'Board' consists of a list of 'Brick's.
  Board {
        -- | A ['Brick'].
        bricks :: [Brick]
        }

instance Visible Board where
  -- | 'render' draws a 'Board' by calling the render function of each 'Brick'
  -- on the 'Board'.
  render (Board bs) = Pictures (map render bs)

brickBoard ::  Int -> Int -> [Brick]
-- | 'brickBoard' makes a list of 'Brick's.
brickBoard w h = brickColumns l b nColumns nRows hSpace vSpace
  where l = negate (div nColumns 2 * hSpace)
        b = negate (div nRows 2 * vSpace)
        nColumns = 1 + div w (hSpace * 2) * 2
        nRows = 1 + div h (vSpace * 2) * 2
        hSpace = 3
        vSpace = 6

brickColumn ::  Int -> Int -> Int -> Int -> [Brick]
-- | 'brickColumn' makes a column of 'Brick's.
brickColumn _ _ 0 _ = []
brickColumn x b nRows distance =
  Brick (fromIntegral x, fromIntegral b) (1, 3) hp hp white
  : brickColumn x (b + distance) (nRows - 1) distance
  where hp = if x == 0
               then 10
               else floor (10 / (log . abs $ fromIntegral x :: Float))

brickColumns ::  Int -> Int -> Int -> Int -> Int -> Int -> [Brick]
-- | 'brickColumns' makes columns of 'Brick's.
brickColumns _ _ 0 _ _ _ = []
brickColumns l b nColumns nRows hDistance vDistance =
  brickColumn l b nRows vDistance
  ++ brickColumns (l + hDistance) b (nColumns - 1) nRows hDistance vDistance
