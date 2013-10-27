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
- along with bwekfwu  If not, see <http://www.gnu.org/licenses/>.
-} module Visible.Brick where

import Graphics.Gloss.Data.Color (Color, mixColors, white)
import Graphics.Gloss.Data.Picture (Picture (Color, Translate)
                                   , rectangleSolid)

import Shape (Shape (AARect), Height, Width)
import Tangible (Tangible, Position, bottom, centre, colour, height, left
                ,right, shape, top, width)
import Visible (Visible, render)

data Brick = Brick Position (Width, Height) Health MaxHealth Color

type Health = Int
type MaxHealth = Int

instance Visible Brick where
  render b =
    Color (mixColors 1.0 (fromIntegral (health b)) (colour b) white)
    $ uncurry Translate (centre b)
    $ rectangleSolid (width b) (height b)

instance Tangible Brick where
  shape (Brick _ (w, h) _ _ _)        = AARect w h
  centre (Brick (x, y) _ _ _ _)       = (x, y)
  left (Brick (x, _) (w, _) _ _ _)    = x - w / 2.0
  right (Brick (x, _) (w, _) _ _ _)   = x + w / 2
  top (Brick (_, y) (_, h) _ _ _)     = y + h / 2.0
  bottom (Brick (_, y) (_, h) _ _ _)  = y - h / 2.0
  width (Brick _ (w, _) _ _ _)        = w
  height (Brick _ (_, h) _ _ _)       = h
  colour (Brick _ _ _ _ c)            = c

health ::  Brick -> Health
health (Brick _ _ h _ _) = h

maxHealth ::  Brick -> Health
maxHealth (Brick _ _ _ h _) = h

updateBrick ::  Brick -> Color -> Maybe Brick
updateBrick (Brick p s h maxH c) col =
  if h' > 1
    then Just (Brick p s h' maxH c')
    else Nothing
  where h' = if c == white || c == col
               then h - 1
               else h + 1
        c'
          | c == white = col
          | maxH == h' = white
          | otherwise  = c
