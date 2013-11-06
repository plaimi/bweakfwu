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
  -- Note that brick ownership is determined by comparing the Brick's Color
  -- to the Color of the 'Paddle's/'Ball's of the players. In the future this
  -- should be done more sanely.
  Brick Position (Width, Height) Health MaxHealth Color

-- | 'Health' is the number of hit points an object has.
type Health = Int
-- | 'MaxHealth' is the max number of hit points an object can possibly have.
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
  shape (Brick _ (w, h) _ _ _)        = AARect w h
  -- | 'centre' is the centre 'Point' of a 'Brick'.
  centre (Brick (x, y) _ _ _ _)       = (x, y)
  -- | 'left' is the horizontal of the left side of a 'Brick'.
  left (Brick (x, _) (w, _) _ _ _)    = x - w / 2.0
  -- | 'right' is the horizontal of the right side of a 'Brick'.
  right (Brick (x, _) (w, _) _ _ _)   = x + w / 2
  -- | 'top' is the vertical of the top side of a 'Brick'.
  top (Brick (_, y) (_, h) _ _ _)     = y + h / 2.0
  -- | 'bottom' is the vertical of the bottom side of a 'Brick'.
  bottom (Brick (_, y) (_, h) _ _ _)  = y - h / 2.0
  -- | 'width' is the 'Width' of a 'Brick'.
  width (Brick _ (w, _) _ _ _)        = w
  -- | 'height' is the 'Height' of a 'Brick'.
  height (Brick _ (_, h) _ _ _)       = h
  -- | 'colour' is the 'Color' of a 'Brick'.
  colour (Brick _ _ _ _ c)            = c

health ::  Brick -> Health
-- | 'health' gets the 'Health' of a 'Brick'.
health (Brick _ _ h _ _) = h

maxHealth ::  Brick -> Health
-- | 'maxHealth' gets the 'MaxHealth' of a 'Brick'.
maxHealth (Brick _ _ _ h _) = h
