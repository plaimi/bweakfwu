{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
- Copyright (C) 2013 Stian Ellingsen <stian@plaimi.net>
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
-} module Tangible where

import Graphics.Gloss.Data.Color (Color)

import Shape (Shape, Height, Normal, Width, intersect)
import Visible (Visible)

class (Visible a) => Tangible a where
  shape  ::  a -> Shape
  centre ::  a -> (Float, Float)
  left   ::  a -> Float
  right  ::  a -> Float
  top    ::  a -> Float
  bottom ::  a -> Float
  width  ::  a -> Float
  height ::  a -> Float
  colour ::  a -> Color

type Position = (Width, Height)

collide ::  (Tangible a, Tangible b) => a -> b -> Maybe Normal
collide a b = intersect (shape a) (shape b) (centre b - centre a)
