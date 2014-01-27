{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
- Copyright (C) 2013 Stian Ellingsen <stian@plaimi.net>
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
-} module Vector where

import Graphics.Gloss.Data.Vector (Vector, dotV, magV)

infixl 7 ^.^
(^.^) ::  Vector -> Vector -> Float
-- | '^.^' finds the scalar (dot) product of two 2D 'Vector's. (I.e. the
-- product of the magnitudes and the cosine of the angle between them.)
(^.^) = dotV

vecScalarOp :: (Float -> Float -> Float) -> Vector -> Float -> Vector
-- | 'vecScalarOp' takes a binary mathematical operator and applies it to
-- a number and both components of a 2D 'Vector'.
vecScalarOp op (x, y) s = (op x s, op y s)

infixl 7 ^/^
(^/^) ::  Vector -> Float -> Vector
-- | '^/^' divides both components of a 2D 'Vector' with a number.
(^/^) = vecScalarOp (/)

infixl 7 ^*^
-- | '^*^' multiplies both components of a 2D 'Vector' with a number.
(^*^) ::  Vector -> Float -> Vector
(^*^) = vecScalarOp (*)

magVec ::  Vector -> Float
-- | 'magVec' finds the magnitude of a 2D 'Vector'.
magVec = magV

vecLimitMag ::  Float -> Vector -> Vector
-- | 'vecLimitMag' clamps the magnitude of a 2D 'Vector' to an argument.
vecLimitMag maxMag v
  | m > maxMag = v ^*^ (maxMag / m)
  | otherwise  = v
  where m = magVec v

vecNorm ::  Vector -> Vector
-- | 'vecNorm' normalises a 2D 'Vector'.
vecNorm v
  | v == (0, 0) = v
  | otherwise = v ^*^ (1 / magVec v)
