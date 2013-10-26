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
-} module World where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

import Graphics.Gloss.Data.Color (magenta, white, yellow)
import Graphics.Gloss.Data.Picture (Picture (Color, Pictures, Scale)
                                   , rectangleWire)

import Movable (move, reflect, vel)
import Movable.Ball (Ball (Ball), collideBall, collideRectangle)
import Movable.Paddle (Paddle (Paddle))
import Tangible (centre, left, right)
import Time (StepTime)
import Vector ((^+^), (^/^))
import Visible (render)
import Visible.ScoreKeeper (ScoreKeeper (ScoreKeeper), Score)
import Visible.Board (Board (Board), brickBoard)
import Visible.Brick (Brick (Brick), updateBrick)
import Window (windowHeight, windowWidth)

data World = World (Paddle, Paddle) (Ball, Ball) Board ScoreKeeper

bang ::  World
bang =
  World (Paddle (-35.0, 0) (1, 5) yellow 0 (False, False, False)
        ,Paddle (35.0, 0) (1, 5) magenta 0 (False, False, False))
        (Ball (-34.0, 0) 0.5 yellow 0
        ,Ball (34.0, 0) 0.5 magenta 0)
        (Board board)
        $ ScoreKeeper 0 0

crunch :: World
crunch = bang

view ::  World -> Picture
view (World (p1, p2) (b1, b2) bs s) =
  Scale worldScale worldScale
  $ Pictures [render p1
             ,render p2
             ,render b1
             ,render b2
             ,render bs
             ,render s
             ,Color white $ rectangleWire 80 45]
  where wh         = fromIntegral windowHeight
        ww         = fromIntegral windowWidth
        worldScale = min (wh / worldHeight) (ww / worldWidth)

step ::  StepTime -> World -> World
step t w =
  if gameP w
    then gameStep t w
    else gameStop

gameP ::  World -> Bool
gameP (World _ _ (Board bs) (ScoreKeeper s1 s2)) =
  not (emptyBoard && winner)
  where emptyBoard = null bs
        winner     = s1 /= s2

gameStop ::  World
gameStop = crunch

gameStep ::  StepTime -> World -> World
gameStep t w = 
  updateVisibles
  $ updateTangibles t
  $ clampTangibles w

board ::  [Brick]
board = brickBoard 45 45

clampTangibles :: World -> World
clampTangibles (World p b bs s) =
  let p'  = clampPaddles p
      b'  = clampBalls b
      bs' = bs
  in  World p' b' bs' s

clampPaddles ::  (Paddle, Paddle) -> (Paddle, Paddle)
clampPaddles (p1, p2) = (clampPaddle p1, clampPaddle p2)

clampPaddle ::  Paddle -> Paddle
clampPaddle p@(Paddle (x, y) (w, h) c vv tv)
  | upTestBounds(y, hh)   = Paddle (x, hw - hh) (w, h) c (vv' (0, 1)) tv
  | downTestBounds(y, hh) = Paddle (x, -hw + hh) (w, h) c (vv' (0, -1)) tv
  | otherwise             = p
  where vv' n = reflect n vv 0
        hh    = h/2
        hw    = worldHeight/2

clampBalls ::  (Ball, Ball) -> (Ball, Ball)
clampBalls (b1, b2) = (clampBall b1, clampBall b2)

clampBall ::  Ball -> Ball
clampBall b@(Ball (x, y) r c (hv, vv))
  | upTestBounds (y, r)   = Ball (x, y) r c (hv, negate vv)
  | downTestBounds (y, r) = Ball (x, y) r c (hv, negate vv)
  | otherwise             = b

updateTangibles ::  StepTime -> World -> World
updateTangibles t (World (p1, p2) (b1, b2) bs s) =
  let p'                    = (move p1 t, move p2 t)
      ((b1', b2'), bs', s') = reflectBricks b1 b2 bs
      b1''                  = reflectPaddles b1' p1 p2
      b2''                  = reflectPaddles b2' p1 p2
      (b1''', b2''')        = reflectBalls b1'' b2''
      b'                    = (move b1''' t, move b2''' t)
  in  World p' b' bs' (mergeScores s s' (ScoreKeeper 0 0))

updateVisibles ::  World -> World
updateVisibles (World (p1, p2) (b1, b2) bs s) =
  let (newB1P, newB2P, s') = updateScores (b1, b2) s
      b1' = if newB1P then resetBall b1 p1 else b1
      b2' = if newB2P then resetBall b2 p2 else b2
      bs' = bs
  in  World (p1, p2) (b1', b2') bs' s'

updateScores ::  (Ball, Ball) -> ScoreKeeper -> (Bool, Bool, ScoreKeeper)
updateScores (b1, b2) s =
  (resetBall1, resetBall2, s')
  where result1    = updateScore b1
        result2    = updateScore b2
        resetBall1 = fst result1
        resetBall2 = fst result2
        s'         = mergeScores s (snd result1) (snd result2)

updateScore ::  Ball -> (Bool, ScoreKeeper)
updateScore (Ball (x, _) r c _)
  | leftTestBounds (x, r/2)  = if c == yellow
                                 then (True, ScoreKeeper (-30) 0)
                                 else (True, ScoreKeeper 0 50)
  | rightTestBounds (x, r/2) = if c == yellow
                                 then (True, ScoreKeeper 50 0)
                                 else (True, ScoreKeeper 0 (-30))
  | otherwise                = (False, ScoreKeeper 0 0)

mergeScores ::  ScoreKeeper -> ScoreKeeper -> ScoreKeeper -> ScoreKeeper
mergeScores (ScoreKeeper s s2) (ScoreKeeper ss ss2) (ScoreKeeper sss sss2) =
  ScoreKeeper (s+ss+sss) (s2+ss2+sss2)

reflectPaddles ::  Ball -> Paddle -> Paddle -> Ball
reflectPaddles b@(Ball p r c v) p1 p2 =
  case cNormal of
    0 -> b
    _ -> Ball p r c (reflect cNormal v cVel)
  where (cNormal, cVel) = fromMaybe (0, 0) (collideRectangle b p1 (vel p1)
                                        <|> collideRectangle b p2 (vel p2))


reflectBricks ::  Ball -> Ball -> Board -> ((Ball, Ball), Board, ScoreKeeper)
reflectBricks b1 b2 (Board bricks)  =
  (( b1', b2'), Board b2bricks, ScoreKeeper s1 s2)
  where (b1', b1bricks, s1) = reflectBricksWithBall b1 bricks
        (b2', b2bricks, s2) = reflectBricksWithBall b2 b1bricks

reflectBricksWithBall ::  Ball -> [Brick] -> (Ball, [Brick], Score)
reflectBricksWithBall b bs = (b', bs', s)
  where
    (b', bs')                = foldr (\x acc -> f
                                      (reflectBrick (fst acc) x) (snd acc))
                                                                 (b, []) bs
    f (ball, Nothing) acc    = (ball, acc)
    f (ball, Just brick) acc = (ball, brick : acc)
    -- Find the max health of all the bricks and sum them.
    sumF                     = sum . map (\(Brick _ _ _ mh _) -> mh)
    sumH                     = sumF bs
    sumH'                    = sumF bs'
    -- If the max health has gone down, this means a brick has been removed.
    -- Award the responsible player with maxHealh^2 points.
    s                        = (sumH - sumH')^(2 :: Int)

reflectBrick ::  Ball -> Brick -> (Ball, Maybe Brick)
reflectBrick ball@(Ball p1 r c v) brick =
  case cNormal of
    0 -> (ball, Just brick)
    _ -> (Ball p1 r c (reflect cNormal v cVel), updateBrick brick c)
  where (cNormal, cVel) = fromMaybe (0, 0) (collideRectangle ball brick 0)


reflectBalls ::  Ball -> Ball -> (Ball, Ball)
reflectBalls b1@(Ball p1 r1 c1 v1) b2@(Ball p2 r2 c2 v2) =
  case cNormal of
    0  -> (b1, b2)
    _  -> (Ball p1 r1 c1 (reflect (negate cNormal) v1 avVel)
          , Ball p2 r2 c2 (reflect cNormal v2 avVel))
    where cNormal = fromMaybe 0 (collideBall b1 b2)
          avVel   = (v1 ^+^ v2) ^/^ 2

leftTestBounds ::  (Float, Float) -> Bool
leftTestBounds (a, b) = a <= (-worldWidth/2) + b/2

rightTestBounds ::  (Float, Float) -> Bool
rightTestBounds (a, b) = a >= worldWidth/2 - b/2

upTestBounds ::  (Float, Float) -> Bool
upTestBounds (a, b) = a >= worldHeight/2 - b

downTestBounds ::  (Float, Float) -> Bool
downTestBounds (a, b) = a <= (-worldHeight/2) + b

resetBall ::  Ball -> Paddle -> Ball
resetBall (Ball _ r c _) p =
  Ball (x, 0) r c (0, 0)
  where x = if c == yellow
              then right p + 0.5
              else left p - 0.5

launchBall ::  Ball -> Paddle -> Ball
launchBall b@(Ball p r c v) paddle =
  case v of
    0 -> Ball p r c (negate (centre paddle)) -- Vel is based on paddle pos.
    _ -> b

worldWidth ::  Float
worldWidth = 80.0

worldHeight ::  Float
worldHeight = 45.0
