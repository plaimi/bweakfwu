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
-} module Geometry where

import Graphics.Gloss.Data.Vector (Vector)

-- | 'Width' is the width of an object.
type Width = Float
-- | 'Height' is the height of an object.
type Height = Float
-- | 'Radius' is the radius of an object.
type Radius = Float
-- | 'Normal' is a normal vector.
type Normal = Vector
