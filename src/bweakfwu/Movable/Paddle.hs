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
-} module Movable.Paddle where

import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Picture (Picture (Color, Translate)
                                   , rectangleSolid)
import Graphics.Gloss.Data.Point (Point)

import Movable (Movable, Speed, Velocity, acceleration, move, vel, targetVel
               ,updateVelocity)
import Rectangle (RectangleSize)
import Tangible (Tangible, Position, bottom, centre
                , colour, height, left, right, top, width)
import Visible (Visible, render)

data Paddle = Paddle Position RectangleSize Color Velocity Controls
data Direction = U | D | L deriving (Eq)

type Controls = (Bool, Bool, Bool)
type ControlInput = (Direction, Bool)
type Edge   = Point

instance Visible Paddle where
  render p =
    Color (colour p)
    $ uncurry Translate (centre p)
    $ rectangleSolid (width p) (height p)

instance Tangible Paddle where
  centre (Paddle (x, y) _ _ _ _) = (x, y)
  left (Paddle (x, _) (w, _) _ _ _)   = x - w / 2.0
  right (Paddle (x, _) (w, _) _ _ _)  = x + w / 2
  top (Paddle (_, y) (_, h) _ _ _)    = y + h / 2.0
  bottom (Paddle (_, y) (_, h) _ _ _) = y - h / 2.0
  width (Paddle _ (w, _) _ _ _)       = w
  height (Paddle _ (_, h) _ _ _)      = h
  colour (Paddle _ _ c _ _)           = c

instance Movable Paddle where
  vel (Paddle (_, _) (_, _) _ v _)    = v

  move p@(Paddle (x, y) (w, h) c v (cu, cd, cl)) dt =
    Paddle (x, y + snd v * dt) (w, h) c (updateVelocity p dt) (cu, cd, cl)

  targetVel (Paddle _ _ _ _ (cu, cd, _)) = (0, tvy cu - tvy cd)
    where tvy False = 0
          tvy True = maxSpeed

  acceleration _ = 300

react ::  Paddle -> ControlInput -> Paddle
react (Paddle pos s c v ctrls) ci = Paddle pos s c v $ updateControls ci ctrls

updateControls ::  ControlInput -> Controls -> Controls
updateControls (U, b) (_, d, l) = (b, d, l)
updateControls (D, b) (u, _, l) = (u, b, l)
updateControls (L, b) (u, d, _) = (u, d, b)

maxSpeed ::  Speed
maxSpeed = 45.0
