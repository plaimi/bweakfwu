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
- along with bweakfwu.  If not, see <http://www.gnu.org/licenses/>.
-} module Window where

import Graphics.Gloss.Data.Display (Display (InWindow))

window ::  Display
-- | 'window' makes an 'InWindow' with a title, size and position.
window = InWindow "bweakfwu" (windowWidth, windowHeight) (10, 10)

windowWidth ::  Int
-- | 'windowWidth' is the 'window' width.
windowWidth = 1280

windowHeight ::  Int
-- | 'windowHeight' is the 'window' height.
windowHeight = 720
