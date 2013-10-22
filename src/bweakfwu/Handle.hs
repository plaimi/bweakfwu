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
-} module Handle where

import Graphics.Gloss.Interface.Pure.Game

import Movable.Paddle
import World

handle :: Event -> World -> World
-- Left paddle up
handle (EventKey (SpecialKey KeyUp) state _ _) (World (p1, p2) b bs s) =
  World (p1, react p2 (U, state == Down)) b bs s

-- Left paddle down
handle (EventKey (SpecialKey KeyDown) state _ _) (World (p1, p2) b bs s) =
  World (p1, react p2 (D, state == Down)) b bs s

-- Right paddle up
handle (EventKey (Char ',') state _ _) (World (p1, p2) b bs s) =
  World (react p1 (U, state == Down), p2) b bs s

-- Right paddle down
handle (EventKey (Char 'o') state _ _) (World (p1, p2) b bs s) =
  World (react p1 (D, state == Down), p2) b bs s

-- Left paddle launch ball
handle (EventKey (Char 'e') Down _ _) (World p@(p1, _) (b1, b2) bs s) =
  World p (launchBall b1 p1, b2) bs s

-- Right paddle launch ball
handle (EventKey (SpecialKey KeyLeft) Down _ _) (World p@(_, p2) (b1, b2) bs s) =
  World p (b1, launchBall b2 p2) bs s

handle _ w = w
