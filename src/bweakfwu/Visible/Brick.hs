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
-} module Visible.Brick where

import Graphics.Gloss.Data.Color (Color, mixColors, white)
import Graphics.Gloss.Data.Picture (Picture (Color, Translate)
                                   , rectangleSolid)

import Geometry (Height, Width)
import Shape (Shape (AARect))
import Tangible (Tangible, Position, bottom, centre, colour, height, left
                ,right, shape, top, width)
import Visible (Visible, render)

data Brick =
  -- | A 'Brick' has a 'Position', a 'Width' and a 'Height', 'Health', a
  -- 'MaxHealth' value and a 'Color'.
  --
  -- Note that 'Brick' ownership is determined by comparing the 'Color' of the
  -- 'Brick' to the 'Color' of the 'Paddle's or 'Ball's of the players. In the
  -- future this should be done more sanely.
  Brick {
         -- | The 'Position' of a 'Brick'.
        position    :: Position,
         -- | The 'Width' and 'Height' of a 'Brick'.
        widthHeight :: (Width, Height),
         -- | The 'Health' of a 'Brick'.
        health      :: Health,
         -- | The 'MaxHealth' of a 'Brick'.
        maxHealth   :: MaxHealth,
         -- | The 'Color' of a 'Brick'.
        color        :: Color
        }

-- | 'Health' is the number of hit points an object has.
type Health = Int
-- | 'MaxHealth' is the max 'Health' value an object can possibly have.
type MaxHealth = Health

instance Visible Brick where
  -- | 'render' draws a 'Brick' in its 'Position'. Its 'Color' is determined
  -- by whether it is owned, and how much 'Health' it has.
  render b =
    Color (mixColors 1.0 (fromIntegral (health b)) (colour b) white)
    $ uncurry Translate (centre b)
    $ rectangleSolid (width b) (height b)

instance Tangible Brick where
  -- | 'shape' is the 'Shape' of a 'Brick'.
  shape    = uncurry AARect . widthHeight
  -- | 'centre' is the centre 'Point' of a 'Brick'.
  centre   = position
  -- | 'left' is the horizontal of the left side of a 'Brick'.
  left b   = fst (position b) - fst (widthHeight b) / 2.0
  -- | 'right' is the horizontal of the right side of a 'Brick'.
  right b  = fst (position b) + fst (widthHeight b) / 2.0
  -- | 'top' is the vertical of the top side of a 'Brick'.
  top b    = snd (position b) + snd (widthHeight b) / 2.0
  -- | 'bottom' is the vertical of the bottom side of a 'Brick'.
  bottom b = snd (position b) - snd (widthHeight b) / 2.0
  -- | 'width' is the 'Width' of a 'Brick'.
  width    = fst . widthHeight
  -- | 'height' is the 'Height' of a 'Brick'.
  height   = snd . widthHeight
  -- | 'colour' is the 'Color' of a 'Brick'.
  colour   = color
