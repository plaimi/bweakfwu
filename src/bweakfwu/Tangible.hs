{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
- Copyright (C) 2013 Stian Ellingsen <stian@plaimi.net>
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
- along with bweakfwu.  If not, see <http://www.gnu.org/licenses/>.
-} module Tangible where

import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Point (Point)

import Geometry (Height, Normal, Width)
import Shape (Shape, intersect)
import Visible (Visible)

-- | All objects in the 'World' that are capable of colliding are 'Tangible'
-- objects. All 'Tangible' objects are 'Visible'.
class (Visible a) => Tangible a where
  -- | 'shape' is the 'Shape' of a 'Tangible'.
  shape  ::  a -> Shape
  -- | 'centre' is the centre point of a 'Tangible'.
  centre ::  a -> Point
  -- | 'left' is the horizontal of the left side of a 'Tangible'.
  left   ::  a -> Float
  -- | 'right' is the horizontal of the right side of a 'Tangible'.
  right  ::  a -> Float
  -- | 'top' is the vertical of the top side of a 'Tangible'.
  top    ::  a -> Float
  -- | 'bottom' is the vertical of the bottom side of a 'Tangible'.
  bottom ::  a -> Float
  -- | 'width' is the 'Width' of a 'Tangible'.
  width  ::  a -> Float
  -- | 'height' is the 'Height' of a 'Tangible'.
  height ::  a -> Float
  -- | 'colour' is the 'Color' of a 'Tangible'.
  colour ::  a -> Color

-- | 'Position' is a ('Width', 'Height').
type Position = (Width, Height)

collide ::  (Tangible a, Tangible b) => a -> b -> Maybe Normal
-- | 'collide' checks if two 'Tangible's intersect.
collide a b = intersect (shape a) (shape b) (centre b - centre a)
