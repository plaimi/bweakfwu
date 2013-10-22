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

import Graphics.Gloss

import Rectangle
import Tangible
import Visible

data Brick = Brick Position RectangleSize Health Color

type Health = Int

instance Visible Brick where
  render b =
    Color (mixColors 1.0 (fromIntegral (health b)) (colour b) white)
    $ uncurry Translate (centre b)
    $ rectangleSolid (width b) (height b)

instance Tangible Brick where
  centre (Brick (x, y) _ _ _)       = (x, y)
  left (Brick (x, _) (w, _) _ _)    = x - w / 2.0
  right (Brick (x, _) (w, _) _ _)   = x + w / 2
  top (Brick (_, y) (_, h) _ _)     = y + h / 2.0
  bottom (Brick (_, y) (_, h) _ _)  = y - h / 2.0
  width (Brick _ (w, _) _ _)        = w
  height (Brick _ (_, h) _ _)       = h
  colour (Brick _ _ _ c)            = c

health ::  Brick -> Health
health (Brick _ _ h _) = h
