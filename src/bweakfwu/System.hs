{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
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
- along with bweakfwu.  If not, see <http://www.gnu.org/licenses/>.
-} module System where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.Pure.Game (Event)

import Time (StepTime)
--
-- | 'RunningP' signifies whether a 'System' is active or not.
type RunningP = Bool

-- | All components that are updated every frame (with 'step', 'handle' and
-- 'draw') are 'System's.
class System a where
  -- | 'initialise' makes the initial 'System'.
  initialise   :: a
  -- | 'draw' redraws a 'System' each frame.
  draw         :: a -> Picture
  -- | 'step' advances the 'System' one frame.
  step         :: StepTime -> a -> a
  -- | 'handle' lets the 'System' react to an 'Event'.
  handle       :: Event -> a -> a
