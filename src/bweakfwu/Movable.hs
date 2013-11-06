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

import Geometry (Normal)
import Time (StepTime)
import Vector ((^*^), (^.^), vecLimitMag)

-- | All objects that are capable of moving in the 'World' are 'Movable'
-- objects.
class Movable a where
  -- | 'vel' is the 'Velocity' of a 'Movable'.
  vel          ::  a -> Velocity
  -- | 'move' steps a 'Movable' one step forward by moving it according to its
  -- 'Velocity'.
  move         ::  a -> StepTime -> a
  -- | 'targetVel' is the target 'Velocity' a 'Movable' wants to achieve.
  targetVel    ::  a -> Velocity
  -- | 'acceleration' is the current 'Acceleration' of a movable.
  acceleration ::  a -> Acceleration

-- | 'Acceleration' is the acceleration of a 'Movable'
type Acceleration = Float
-- | 'Speed' is the speed of a 'Movable'
type Speed = Float
-- | 'Velocity' is the velocity of a 'Movable'
type Velocity = Vector

updateVelocity ::  Movable a => a -> StepTime -> Velocity
-- | 'updateVelocity' updates the 'Velocity' of a 'Movable' based on the step
-- time. It makes sure the Movable adheres to a limit.
updateVelocity m dt = vel m + vecLimitMag (dt * acceleration m) dv
  where dv = targetVel m - vel m

reflect ::  Float -> Normal -> Velocity -> Velocity -> Velocity
-- | 'reflect' calculates a new 'Velocity' based on frictionless collision on
-- the collision 'Normal' with the Velocitys of the two objects that crash.
reflect cor n v w =
  dvn'n + v                      -- New velocity from frictionless collision.
  where dv    = w - v            -- Relative velocity between colliders.
        dc    = dv ^*^ (1 + cor) -- Collision delta as if head-on crash.
        dvn   = n ^.^ dc         -- Velocity delta on collision normal.
        dvn'  = max 0 dvn        -- Sanitation of dvn.
        dvn'n = n ^*^ dvn'       -- Vectorificationing of dvn.
