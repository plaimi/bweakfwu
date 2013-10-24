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
-} module Tangible where

import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Vector (Vector)

import Movable (Velocity)
import Vector ((^+^), (^*^), (^.^))
import Visible (Visible)

class (Visible a) => Tangible a where
  centre ::  a -> (Float, Float)
  left   ::  a -> Float
  right  ::  a -> Float
  top    ::  a -> Float
  bottom ::  a -> Float
  width  ::  a -> Float
  height ::  a -> Float
  colour ::  a -> Color

type Width = Float
type Height = Float
type Radius = Float
type Position = (Width, Height)
type Normal = Vector
type Speed = Float
type Acceleration = Float

reflect ::  Normal -> Velocity -> Velocity -> Velocity
reflect n v w =
  dvn'n ^+^ v                   -- New velocity based on frictionless and
                                -- superelastic collision¹.
                                --
  where k     = 1.01            -- Restitution coefficient for gameplay.
        dv    = w - v           -- Relative velocity between colliders.
        dc    = dv ^*^ (1 + k)  -- Collision delta as if head-on crash.
        dvn   = n ^.^ dc        -- Velocity delta on collision normal
        dvn'  = min 0 dvn       -- Sanitation of dvn
        dvn'n = n ^*^ dvn'      -- Vectorificationing of dvn




























































































































































































































































































































































































































































--¹ AKA troll physics
