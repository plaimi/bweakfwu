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

import Graphics.Gloss.Interface.Pure.Game (Event (EventKey)
                                          , Key (Char, SpecialKey)
                                          , KeyState (Down)
                                          , SpecialKey (KeyDown, KeyLeft
                                                       ,KeySpace, KeyUp))

import Movable.Paddle (react, Direction (U, D))
import World (World (World), launchBall, updateRunning)

handle :: Event -> World -> World
-- | 'handle' handles input 'Event's and makes the 'World' react to them.

-- Left paddle up.
handle (EventKey (SpecialKey KeyUp) state _ _) (World (p1, p2) b bs s r) =
  World (p1, react p2 (U, state == Down)) b bs s r

-- Left paddle down.
handle (EventKey (SpecialKey KeyDown) state _ _) (World (p1, p2) b bs s r) =
  World (p1, react p2 (D, state == Down)) b bs s r

-- Right paddle up.
handle (EventKey (Char 'u') state _ _) (World (p1, p2) b bs s r) =
  World (react p1 (U, state == Down), p2) b bs s r

-- Right paddle down.
handle (EventKey (Char 'j') state _ _) (World (p1, p2) b bs s r) =
  World (react p1 (D, state == Down), p2) b bs s r

-- Left paddle launch ball.
handle (EventKey (Char 'k') Down _ _) (World p@(p1, _) (b1, b2) bs s r) =
  World p (launchBall b1 p1, b2) bs s r

-- Right paddle launch ball.
handle (EventKey (SpecialKey KeyLeft) Down _ _)
       (World p@(_, p2) (b1, b2) bs s r) =
  World p (b1, launchBall b2 p2) bs s r

-- Un/pause game.
handle (EventKey (SpecialKey KeySpace) Down _ _) (World p b bs s r) =
  World p b bs s (updateRunning r)

handle _ w = w
