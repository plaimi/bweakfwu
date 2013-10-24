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
-} module Visible.ScoreKeeper where

import Graphics.Gloss.Data.Color (dark, cyan)
import Graphics.Gloss.Data.Picture (Picture (Color, Scale, Text, Translate))

import Visible (Visible, render)

data ScoreKeeper = ScoreKeeper Score Score

type Score = Int

instance Visible ScoreKeeper where
  render s =
    Color (dark cyan)
    $ Translate (-5.0) 18.0
    $ Scale 0.03 0.03
    $ Text (stringScore s)

stringScore ::  ScoreKeeper -> String
stringScore (ScoreKeeper s1 s2) = show s1 ++ " : " ++ show s2
