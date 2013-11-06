{-
- Copyright (C) 2013 Stian Ellingsen <stian@plaimi.net>
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
- along with bwekfwu  If not, see <http://www.gnu.org/licenses/>.
-} module Mathema where

magApply ::  (Float -> Float) -> Float -> Float
-- | 'magApply' applies a function to the magnitude of a number.
magApply f x =
  let sig = signum x
  in  sig * f (sig * x)

clamp ::  Float -> Float -> Float -> Float
-- | 'clamp' clamps a number between a low and a high.
clamp low high = max low . min high
