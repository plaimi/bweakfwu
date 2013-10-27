{-
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
-} module Shape where

import Graphics.Gloss.Data.Vector (Vector)

import Mathema (clamp)
import Vector (magVec, vecNorm)

data Shape = Circle Radius | AARect Width Height

type Width = Float
type Height = Float
type Radius = Float
type Normal = Vector

outerRadius ::  Shape -> Radius
outerRadius (Circle r) = r
outerRadius (AARect w h) = magVec (w, h) / 2

intersect ::  Shape -> Shape -> Vector -> Maybe Normal
-- Circle--AARect is like AARect--Circle. In principle, both the vector
-- parameter and the resulting normal should be negated, but leaving them both
-- as they are is just as effective.
intersect c@(Circle _) r@(AARect _ _) v = intersect r c v
-- Fast check to eliminate pairs that don't have overlapping bounding circles.
intersect s1 s2 v
  | magVec v > outerRadius s1 + outerRadius s2 = Nothing
-- For Circle--Circle, the radius check was enough, so just return the normal.
intersect (Circle _) (Circle _) v = Just (vecNorm v)
-- AARect--Circle
intersect (AARect w h) (Circle r) v@(x, y)
  | magVec pv > r  = Nothing
  | otherwise      = Just (vecNorm pv)
  where
    p  = (clamp (-hw) hw x, clamp (-hh) hh y)
    pv = v - p
    hw = w / 2
    hh = h / 2
