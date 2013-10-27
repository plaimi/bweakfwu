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
-} module Vector where

import Graphics.Gloss.Data.Vector (Vector, dotV, magV)

infixl 7 ^.^
(^.^) ::  Vector -> Vector -> Float
(^.^) = dotV

vecScalarOp :: (Float -> Float -> Float) -> Vector -> Float -> Vector
vecScalarOp op (x, y) s = (op x s, op y s)

infixl 7 ^/^
(^/^) ::  Vector -> Float -> Vector
(^/^) = vecScalarOp (/)

infixl 7 ^*^
(^*^) ::  Vector -> Float -> Vector
(^*^) = vecScalarOp (*)

magVec ::  Vector -> Float
magVec = magV

vecLimitMag ::  Float -> Vector -> Vector
vecLimitMag maxMag v
  | m > maxMag = v ^*^ (maxMag / m)
  | otherwise  = v
  where m = magVec v

vecNorm ::  Vector -> Vector
vecNorm v
  | v == (0, 0) = v
  | otherwise = v ^*^ (1 / magVec v)
