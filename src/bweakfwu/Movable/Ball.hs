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

import Control.Monad (msum)

import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Picture (Picture (Color, Translate), circleSolid)

import Mathema (magApply)
import Movable (Movable, Speed, Velocity, acceleration, move, targetVel
               ,updateVelocity, vel)
import Rectangle (collideEdges, collideCorners)
import Tangible (Tangible, Normal, Position, Radius, bottom, centre, colour
                ,height, left, right, top, width)
import Vector ((^-^), (^/^), magVec, vecLimitMag)
import Visible (Visible, render)

data Ball = Ball Position Radius Color Velocity

instance Visible Ball where
  render (Ball p r c _) =
    Color c
    $ uncurry Translate p
    $ circleSolid r

instance Tangible Ball where
  centre (Ball p _ _ _)      = p
  left (Ball (x, _) r _ _)   = x - r
  right (Ball (x, _) r _ _)  = x + r
  top (Ball (_, y) r _ _)    = y + r
  bottom (Ball (_, y) r _ _) = y - r
  width (Ball _ r _  _)      = 2.0 * r
  height (Ball _ r _  _)     = 2.0 * r
  colour (Ball _ _ c _)      = c

instance Movable Ball where
  vel (Ball _ _ _ v)         = v

  move b@(Ball (x, y) c r v) t =
    Ball (x + fst v * t, y + snd v * t) c r (updateVelocity b t)

  targetVel b = vecLimitMag maxSpeed (tvx, tvy)
    where tvx      = magApply (max minHSpeed) vx
          tvy      = magApply (max minVSpeed) vy
          (vx, vy) = vel b

  acceleration = (* accelFactor) . magVec . vel

collideRectangle ::
  Tangible a => Ball -> a -> Velocity -> Maybe (Normal, Velocity)
collideRectangle b r v =
  msum [fmap (flip (,) v) (f b r) |
       f <- [collideEdges, collideCorners]]

collideBall ::  Ball -> Ball -> Maybe Normal
collideBall b1 b2 =
  if distance < ballRadii
    then Just (distanceVector ^/^ distance)
    else Nothing
  where distanceVector = centre b1 ^-^ centre b2
        distance   = magVec distanceVector
        ballRadii  = width b1/2 + width b2/2

maxSpeed ::  Speed
maxSpeed = 150.0

minHSpeed ::  Speed
minHSpeed = 5.0

minVSpeed ::  Speed
minVSpeed = 1.0

accelFactor ::  Float
accelFactor = 1.0 / 5.0
