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

import Control.Monad (msum)

import Graphics.Gloss.Data.Point (Point)

import Tangible (Tangible, Normal, bottom, centre, left, right, top, width)
import Vector ((^-^), (^/^), magVec)

type Corner = Point
type Edge   = Point
type RectangleSize = (Float, Float)

corners ::  Tangible a => a -> [Corner]
corners p =
  [(left p, top p)
  ,(left p, bottom p)
  ,(right p, top p)
  ,(right p, bottom p)]

collideEdges ::  Tangible a => Tangible b => a -> b -> Maybe Normal
collideEdges a b
  | snd (centre a) < top b &&
    snd (centre a) > bottom b &&
    right a > left b &&
    left a < right b = Just (signum (fst (centre b) - fst (centre a)) , 0.0)
  | fst (centre a) < right b &&
    fst (centre a) > left b &&
    top a > bottom b &&
    bottom a < top b = Just (0.0, signum (snd (centre b) - snd (centre a)))
  | otherwise        = Nothing

collideCorners ::  Tangible a => Tangible b => a -> b -> Maybe Normal
collideCorners a b = msum (map (collideCorner a) (corners b))

collideCorner ::  Tangible a => a -> Corner -> Maybe Normal
collideCorner a c =
  if distance < ballRadius
    then Just (distanceVector ^/^ distance)
    else Nothing
  where distanceVector = c ^-^ centre a
        distance       = magVec distanceVector
        ballRadius     = width a/2
