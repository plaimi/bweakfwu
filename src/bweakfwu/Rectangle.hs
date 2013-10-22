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
-} module Rectangle where

import Graphics.Gloss

import Tangible

type Corner = Point
type Edge   = Point
type RectangleSize = (Float, Float)

corners ::  Tangible a => a -> [Corner]
corners p =
  [(left p, top p)
  ,(left p, bottom p)
  ,(right p, top p)
  ,(right p, bottom p)]
