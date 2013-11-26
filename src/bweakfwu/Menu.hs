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
-} module Menu where

import Control.Arrow ((***))
import Control.Monad (join)

import Graphics.Gloss.Data.Color (black, dim, white)
import Graphics.Gloss.Data.Picture (Picture (Color, Pictures, Scale, Text
                                   ,Translate), rectangleSolid
                                   ,rectangleWire)
import Graphics.Gloss.Interface.Pure.Game (Event (EventKey), Key (SpecialKey)
                                          ,KeyState (Down)
                                          ,SpecialKey (KeyDown, KeySpace
                                                      ,KeyUp))

import System (System, draw, handle, initialise, step)
import Window (windowHeight, windowWidth)

-- | 'ButtonGraphic' is the 'Picture' of a 'Button'.
type ButtonGraphic = Picture
-- | 'ButtonAction' is a function associated to a 'Button'.
type ButtonAction = Menu -> Menu

data Button =
  -- | A 'Button' is constructed with a 'ButtonAction' and a 'ButtonGraphic'.
  Button {
         -- | The 'ButtonAction' of a button.
         action  :: ButtonAction,
         -- | The 'ButtonGraphic' of a button.
         graphic :: ButtonGraphic
         }

data Menu =
  -- | A 'Menu' is constructed with a set of 'Bool's for whether it is
  -- running or not, whether a 'World' exists or not, whether a new 'World'
  -- should be constructed or not, whether a 'World' should be continued
  -- (and therefore a 'Menu' closed) or not. Lastly it also takes an 'Int' for
  -- what position 'Button' is currently active in the 'Menu'.
  Menu {
       -- | Whether the 'Menu' should be drawn or not.
       runningMenu    :: Bool,
       -- | Whether a 'World' is ongoing or not.
       inGameP        :: Bool,
       -- | Whether a new 'World' should be constructed or not.
       newGameP       :: Bool,
       -- | Whether a 'World' should take precedence, hiding the 'Menu' or
       -- not.
       continueGameP  :: Bool,
       -- | Whether the user has requested to exit the program ar not.
       exitP          :: Bool,
       -- | The position of the currently active 'Button' in a 'Menu'.
       activeButton   :: Int
       }

instance System Menu where
  initialise =
    Menu {
         runningMenu   = True
        ,inGameP       = False
        ,newGameP      = False
        ,exitP         = False
        ,continueGameP = False
        ,activeButton  = 1
         }

  draw m = Pictures (map graphic (buttons m))

  step _ m = m

  handle (EventKey (SpecialKey KeySpace) Down _ _) m =
    action ((!!) (buttons m) (activeButton m - 1)) m

  handle (EventKey (SpecialKey KeyDown) Down _ _) m =
    setActive m (activeButton m + 1)

  handle (EventKey (SpecialKey KeyUp) Down _ _) m =
    setActive m (activeButton m - 1)

  handle _ m = m


buttons :: Menu -> [Button]
-- | 'buttons' is a ['Button']. It contains the 'Button's a 'Menu' has at any
-- given time.
buttons m
  | inGameP m = inGameButtons
  | otherwise = outGameButtons
  where inGameButtons  = [Button continueGame (mbg "continue" 1 inTotal)
                         ,Button newGame (mbg "new game" 2 inTotal)
                         ,Button exitGame (mbg "exit" 3 inTotal)]
        outGameButtons = [Button newGame (mbg "start" 1 outTotal)
                         ,Button exitGame (mbg "exit" 2 outTotal)]
        inTotal        = 3
        outTotal       = 2
        mbg            = makeButtonGraphic m


makeButtonGraphic ::  Menu -> String -> Int -> Int -> Picture
-- | 'makeButtonGraphic' makes a 'Button' in a 'Menu'. It needs a 'String' for
-- a label, an Int for the position of the 'Button', and an 'Int' for the
-- total number of 'Button's in the 'Menu'. This function is awful. Don't
-- bother reading it unless you have to change it. It works. Trust me.
makeButtonGraphic m txt pos tot =
  Translate 0.0 placement
  $  Pictures (if active then activeBut else but)
  where but                         = [box, text]
        activeBut                   = but ++ [border]
        box                         = Color backCol
                                      $ rectangleSolid w h
        border                      = Color bordCol
                                      $ rectangleWire w h
        text                        = Color textCol
                                      $ uncurry Translate textPlacement
                                      $ Scale 0.75 0.75
                                      $ Text txt
        (w, h)                      = join (***) fromIntegral
                                      (windowWidth - 500
                                      ,windowHeight - 620)
                                      :: (Float, Float)
        placement                   = (2.0 / total * ((1.0 + total)
                                                      / 2.0 - position))
                                      * (fromIntegral windowHeight / 2)
        textPlacement               = (-w / 2.0, -h / 4.0)
        (total, position)           = join (***) fromIntegral (tot, pos)
        active                      = pos == activeButton m
        (backCol, textCol, bordCol) = (black, white, (dim . dim) white)

setActive ::  Menu -> Int -> Menu
-- | 'setActive' makes a 'Menu' with the 'activeButton' set to the 'Int' that
-- is passed to the function. 'setActive' makes sure that the 'Int' is within
-- the range of the total number of buttons in a 'Menu'.
setActive m@(Menu r i n c e _) b
  | b <= length (buttons m) && b > 0 = Menu r i n c e b
  | otherwise                        = m

newGame :: Menu -> Menu
-- | 'newGame' makes a 'Menu' with a desire to make a new 'World'.
newGame _ =
    Menu {
         runningMenu   = False
        ,inGameP       = False
        ,newGameP      = True
        ,continueGameP = False
        ,exitP         = False
        ,activeButton  = 1
         }

exitGame :: Menu -> Menu
-- | 'exitGame' makes a 'Menu' with a desire to exit the program.
exitGame _ =
    Menu {
         runningMenu   = False
        ,inGameP       = False
        ,newGameP      = False
        ,continueGameP = False
        ,exitP         = True
        ,activeButton  = 1
         }

continueGame :: Menu -> Menu
-- | 'continueGame' makes a 'Menu' with a desire to let the 'World' be drawn.
continueGame _ =
    Menu {
         runningMenu   = False
        ,inGameP       = True
        ,newGameP      = False
        ,continueGameP = True
        ,exitP         = False
        ,activeButton  = 1
         }
