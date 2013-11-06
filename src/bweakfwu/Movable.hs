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
-} module Movable where

import Graphics.Gloss.Data.Vector (Vector)

import Time (StepTime)
import Shape (Normal)
import Vector ((^*^), (^.^), vecLimitMag)

class Movable a where
  vel          ::  a -> Velocity
  move         ::  a -> StepTime -> a
  targetVel    ::  a -> Velocity
  acceleration ::  a -> Acceleration

type Acceleration = Float
type Speed = Float
type Velocity = Vector

updateVelocity ::  Movable a => a -> StepTime -> Velocity
updateVelocity m dt = vel m + vecLimitMag (dt * acceleration m) dv
  where dv = targetVel m - vel m

reflect ::  Float -> Normal -> Velocity -> Velocity -> Velocity
reflect cor n v w =
  dvn'n + v                      -- New velocity from frictionless collision.
  where dv    = w - v            -- Relative velocity between colliders.
        dc    = dv ^*^ (1 + cor) -- Collision delta as if head-on crash.
        dvn   = n ^.^ dc         -- Velocity delta on collision normal.
        dvn'  = max 0 dvn        -- Sanitation of dvn.
        dvn'n = n ^*^ dvn'       -- Vectorificationing of dvn.
