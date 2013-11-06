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
-} module Movable.Ball where

import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Picture (Picture (Color, Translate), circleSolid)

import Geometry (Radius)
import Mathema (magApply)
import Movable (Movable, Speed, Velocity, acceleration, move, targetVel
               ,updateVelocity, vel)
import Shape (Shape (Circle))
import Tangible (Tangible, Position, bottom, centre, colour
                ,height, left, right, shape, top, width)
import Vector (magVec, vecLimitMag)
import Visible (Visible, render)

data Ball =
  -- | 'Ball' has a 'Position', 'Radius', 'Color' and 'Velocity'.
  Ball Position Radius Color Velocity

instance Visible Ball where
  -- | 'render' draws a 'Ball'.
  render (Ball p r c _) =
    Color c
    $ uncurry Translate p
    $ circleSolid r

instance Tangible Ball where
  -- | 'shape' is the 'Shape' of a 'Ball'.
  shape (Ball _ r _ _)       = Circle r
  -- | 'centre' is the centre 'Point' of a 'Ball'.
  centre (Ball p _ _ _)      = p
  -- | 'left' is the horizontal of the left side of a 'Ball'.
  left (Ball (x, _) r _ _)   = x - r
  -- | 'right' is the horizontal of the right side of a 'Ball'.
  right (Ball (x, _) r _ _)  = x + r
  -- | 'top' is the vertical of the top side of a 'Ball'.
  top (Ball (_, y) r _ _)    = y + r
  -- | 'bottom' is the vertical of the bottom side of a 'Ball'.
  bottom (Ball (_, y) r _ _) = y - r
  -- | 'width' is the 'Width' of a 'Ball'.
  width (Ball _ r _  _)      = 2.0 * r
  -- | 'height' is the 'Height' of a 'Ball'.
  height (Ball _ r _  _)     = 2.0 * r
  -- | 'colour' is the 'Color' of a 'Ball'.
  colour (Ball _ _ c _)      = c

instance Movable Ball where
  -- | 'vel' is the 'Velocity' of a 'Ball'.
  vel (Ball _ _ _ v)         = v

  -- | 'move' steps a 'Ball' one step forward by moving it according to its
  -- 'Velocity'.
  move b@(Ball (x, y) c r v) t =
    Ball (x + fst v * t, y + snd v * t) c r (updateVelocity b t)

  -- | 'targetVel' is the target 'Velocity' a 'Ball' wants to achieve.
  targetVel b = vecLimitMag maxSpeed (tvx, tvy)
    where tvx      = magApply (max minHSpeed) vx
          tvy      = magApply (max minVSpeed) vy
          (vx, vy) = vel b

  -- | 'acceleration' calculates the 'Acceleration' of a 'Ball' based on its
  -- 'Speed'.
  acceleration = (* accelFactor) . magVec . vel

maxSpeed ::  Speed
-- | 'maxSpeed' is the maximum 'Speed' a 'Ball' tends towards in any
-- direction.
maxSpeed = 150.0

minHSpeed ::  Speed
-- | 'minHSpeed' is the minimum horizontal 'Speed' a 'Ball' tends towards.
minHSpeed = 10.0

minVSpeed ::  Speed
-- | 'minVSpeed' is the minimum vertical 'Speed' a 'Ball' tends towards.
minVSpeed = 1.0

accelFactor ::  Float
-- | 'accelFactor' is how quickly a 'Ball' 'accelerate's. This determines how
-- quickly the Ball is forced to minimum and maximum speeds.
accelFactor = 1.0 / 2.5
