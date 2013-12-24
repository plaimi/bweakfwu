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
  -- | A 'Ball' has a 'Position', 'Radius', 'Color' and 'Velocity'.
  Ball {
       -- | The 'Position' of a 'Ball'.
       position :: Position,
       -- | The 'Radius' of a 'Ball'.
       radius   :: Radius,
       -- | The 'Color' of a 'Ball'.
       color    :: Color,
       -- | The 'Velocity' of a 'Ball'.
       velocity :: Velocity
       }

instance Visible Ball where
  -- | 'render' draws a 'Ball'.
  render (Ball p r c _) =
    Color c
    $ uncurry Translate p
    $ circleSolid r

instance Tangible Ball where
  -- | 'shape' is the 'Shape' of a 'Ball'.
  shape b  = Circle $ radius b
  -- | 'centre' is the centre 'Point' of a 'Ball'.
  centre   = position
  -- | 'left' is the horizontal of the left side of a 'Ball'.
  left b   = fst (position b) - radius b
  -- | 'right' is the horizontal of the right side of a 'Ball'.
  right b  = fst (position b) + radius b
  -- | 'top' is the vertical of the top side of a 'Ball'.
  top b    = snd (position b) + radius b
  -- | 'bottom' is the vertical of the bottom side of a 'Ball'.
  bottom b = snd (position b) - radius b
  -- | 'width' is the 'Width' of a 'Ball'.
  width b  = 2.0 * radius b
  -- | 'height' is the 'Height' of a 'Ball'.
  height b = 2.0 * radius b
  -- | 'colour' is the 'Color' of a 'Ball'.
  colour   = color

instance Movable Ball where
  -- | 'vel' is the 'Velocity' of a 'Ball'.
  vel      = velocity

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
-- quickly the 'Ball' is forced to minimum and maximum 'Speed's.
accelFactor = 1.0 / 2.5
