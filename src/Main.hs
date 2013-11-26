{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
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
-} module Main where

import Graphics.Gloss.Interface.Pure.Game (play)
import Graphics.Gloss.Data.Color (black)

import Game
import System (draw, initialise, handle, step)
import Window (window)

main ::  IO ()
-- | 'main' starts the game loop.
main =
  play
  window               -- The display mode.
  black                -- The background colour.
  600                  -- The number of simulation steps per second.
  (initialise :: Game) -- Create initial world.
  draw                 -- Convert the world to a picture.
  handle               -- Handle input.
  step                 -- Step the world one iteration.
