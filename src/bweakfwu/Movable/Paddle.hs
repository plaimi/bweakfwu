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

import Geometry (Height, Width)
import Movable (Movable, Speed, Velocity, acceleration, move, vel, targetVel
               ,updateVelocity)
import Shape (Shape (AARect))
import Tangible (Tangible, Position, bottom, centre
                , colour, height, left, right, shape, top, width)
import Visible (Visible, render)

data Paddle =
  -- | A 'Paddle' has a 'Position', a 'Width' and a 'Height', a 'Color', and
  -- a set of 'Controls'.
  Paddle Position (Width, Height) Color Velocity Controls
data Direction =
  -- | 'Direction' is the direction in which a 'Paddle' may move. It may move
  -- up (U), down (D) or left (L).
  U | D | L deriving (Eq)

-- | 'Controls' are three booleans signifying whether the 'Controls' are
-- pressed.
type Controls = (Bool, Bool, Bool)
-- | 'ControllInput' is a whether a 'Paddle' wants to go in a 'Direction'.
type ControlInput = (Direction, Bool)
-- | 'Edge' is the 'Edge' of a 'Paddle'.
type Edge   = Point

instance Visible Paddle where
  -- | 'render' draws a 'Paddle'.
  render p =
    Color (colour p)
    $ uncurry Translate (centre p)
    $ rectangleSolid (width p) (height p)

instance Tangible Paddle where
  -- | 'shape' is the 'Shape' of a 'Paddle'.
  shape (Paddle _ (w, h) _ _ _)       = AARect w h
  -- | 'centre' is the centre 'Point' of a 'Paddle'.
  centre (Paddle (x, y) _ _ _ _)      = (x, y)
  -- | 'left' is the horizontal of the left side of a 'Paddle'.
  left (Paddle (x, _) (w, _) _ _ _)   = x - w / 2.0
  -- | 'right' is the horizontal of the right side of a 'Paddle'.
  right (Paddle (x, _) (w, _) _ _ _)  = x + w / 2
  -- | 'top' is the vertical of the top side of a 'Paddle'.
  top (Paddle (_, y) (_, h) _ _ _)    = y + h / 2.0
  -- | 'bottom' is the vertical of the bottom side of a 'Paddle'.
  bottom (Paddle (_, y) (_, h) _ _ _) = y - h / 2.0
  -- | 'width' is the 'Width' of a 'Paddle'.
  width (Paddle _ (w, _) _ _ _)       = w
  -- | 'height' is the 'Height' of a 'Paddle'.
  height (Paddle _ (_, h) _ _ _)      = h
  -- | 'colour' is the 'Color' of a 'Paddle'.
  colour (Paddle _ _ c _ _)           = c

instance Movable Paddle where
  -- | 'vel' is the 'Velocity' of a 'Paddle'.
  vel (Paddle (_, _) (_, _) _ v _)    = v

  -- | 'move' steps a 'Paddle' one step forward by moving it according to its
  -- 'Velocity'.
  move p@(Paddle (x, y) (w, h) c v (cu, cd, cl)) dt =
    Paddle (x, y + snd v * dt) (w, h) c (updateVelocity p dt) (cu, cd, cl)

  -- | 'targetVel' is the target 'Velocity' a 'Paddle' wants to achieve.
  targetVel (Paddle _ _ _ _ (cu, cd, _)) = (0, tvy cu - tvy cd)
    where tvy False = 0
          tvy True = maxSpeed

  -- | 'acceleration' is the 'Acceleration' of a 'Paddle'.
  acceleration _ = 600

react ::  Paddle -> ControlInput -> Paddle
-- | 'react' lets a 'Paddle' react to 'ControlInput'.
react (Paddle pos s c v ctrls) ci = Paddle pos s c v $ updateControls ci ctrls

updateControls ::  ControlInput -> Controls -> Controls
-- | 'updateControls' updates the 'Controls' based on 'ControlInput'. It
-- checks if the player interacts with the up, down or left key, and whether
-- it is pressing or depressing the key.
updateControls (U, pressP) (_, d, l) = (pressP, d, l)
updateControls (D, pressP) (u, _, l) = (u, pressP, l)
updateControls (L, pressP) (u, d, _) = (u, d, pressP)

maxSpeed ::  Speed
-- | 'maxSpeed' is the maximum 'Speed' a 'Paddle' can have in any direction.
maxSpeed = 60.0
