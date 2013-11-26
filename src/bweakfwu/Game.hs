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
-} module Game where

import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Data.Picture (Picture (Color, Pictures, Text
                                            ,Translate))

import Menu (Menu (Menu), activeButton, continueGameP, exitP, inGameP
            ,newGameP, runningMenu)
import System (System, draw, handle, initialise, step)
import World (World (World), runningWorld)

data Game =
  -- | A 'Game' is constructed with a 'Maybe' 'World', a 'Menu' and
  -- a 'Bool' that signifies whether to exit the program or not.
  Game {
       -- | The 'Maybe' 'World' of a 'Game'.
       world  :: Maybe World,
       -- | The 'Menu' of a 'Game'.
       menu   :: Menu,
       -- | Whether to exit the program or not.
       exit   :: Bool
       }

instance System Game where

  initialise = Game {
                    world = Nothing
                   ,menu  = System.initialise
                   ,exit  = False
                    }

  draw g@(Game Nothing m _) =
    if (not . exit) g
      then System.draw m
      else exitPic

  draw g@(Game (Just w) m _) =
    if (not . exit) g
      then Pictures systems
      else exitPic
    where systems = if runningMenu m
                      then [System.draw w
                           ,System.draw m]
                      else [System.draw w]

  handle e g@(Game Nothing m _)  =
    Game Nothing (if (not . exit) g then System.handle e m else m) (exitP m)

  handle e (Game (Just w) m _) =
    Game (Just w') m' (exitP m)
    where m' = if runningMenu m
                 then System.handle e m
                 else m
          w' = if runningWorld w
                 then System.handle e w
                 else w

  step _ (Game Nothing m _)
    | newGameP m = Game {
                      world = Just System.initialise
                      ,menu  = Menu False True False False False 1
                      ,exit  = False
                      }
    | otherwise  = Game Nothing m (exitP m)
  step t (Game (Just w@(World ps bs b s _)) m _) =
    Game (Just w') m' e
    where w'
            | runningWorld w  = System.step t w
            | continueGameP m = System.step t (World ps bs b s True)
            | newGameP m      = System.step t System.initialise
            | otherwise       = w
          m'
            | exitP m                = m
            | (not . runningWorld) w =
              if not $ runningMenu m
                then Menu {
                          runningMenu    = True
                          ,inGameP       = True
                          ,newGameP      = False
                          ,exitP         = False
                          ,continueGameP = False
                          ,activeButton  = 1
                          }
                else if continueGameP m
                       then Menu {
                                 runningMenu    = False
                                 ,inGameP       = True
                                 ,newGameP      = False
                                 ,exitP         = False
                                 ,continueGameP = False
                                 ,activeButton  = 1
                                 }
                       else m
            | otherwise = Menu False True False False False 1
          e             = exitP m

exitPic :: Picture
-- | Temporary "quit" function that returns a 'Picture'.
exitPic =
  Color white
  $ Translate (-610) 0
  $ Text "press esc to close"
