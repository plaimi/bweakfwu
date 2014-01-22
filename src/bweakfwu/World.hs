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
- along with bwekfwu  If not, see <http://www.gnu.org/licenses/>.
-} module World where

import Control.Applicative ((<$>), (<|>))

import Graphics.Gloss.Data.Color (magenta, white, yellow)
import Graphics.Gloss.Data.Picture (Picture (Color, Pictures, Scale)
                                   , rectangleWire)
import Graphics.Gloss.Data.Point (Point)

import Geometry (Height, Width)
import Movable (dvApply, dvMag, move, reflect, vel)
import Movable.Ball (Ball (Ball))
import Movable.Paddle (Paddle (Paddle))
import Tangible (centre, collide, colour, left, right)
import Time (StepTime)
import Vector ((^/^))
import Visible (render)
import Visible.Board (Board (Board), brickBoard, bricks)
import Visible.Brick (Brick (Brick), maxHealth)
import Visible.ScoreKeeper (ScoreKeeper (ScoreKeeper), Score, mergeScores
                           ,score1, score2)
import Window (windowHeight, windowWidth)

data World =
  -- | A 'World' is constructed with two 'Paddle's, two 'Ball's, a
  -- 'ScoreKeeper' and a 'RunningP' to signify whether it is running or not.
  World {
        -- | Two 'Paddle's, one per player.
        paddles     :: (Paddle, Paddle),
        -- | Two 'Ball's, one per player.
        balls       :: (Ball, Ball),
        -- | The 'Board' of ['Brick'].
        board       :: Board,
        -- | The 'ScoreKeeper' keeps the 'Score's of each player.
        scorekeeper :: ScoreKeeper,
        -- | A 'RunningP' denotes whether the 'World' is running or not.
        runningP    :: RunningP
        }

-- | 'RunningP' signifies whether a 'World' is active or not.
type RunningP = Bool

bang ::  World
-- | 'bang' creates a world.
--
-- A 'World' has two 'Paddle's. One at the left edge, one at the right edge.
-- Each player has its own 'Ball', so a 'World' has two 'Ball's.
--
-- A 'World' has a 'Board', which is a set of 'Brick's. In the future it
-- should be possible to select between different 'Board's.
--
-- A 'World' has a 'ScoreKeeper' that tracks and displays player 'Score's.
--
-- A 'World' has a 'RunningP' that dictates whether it is active or not.
-- To pause the game, this may be set to False.
--
-- What 'Ball' is owned by which player is presently signified by checking the
-- 'Color' of the 'Ball'. In the future, 'Ball' ownership should be
-- introduced.
--
-- 'Brick' ownership is also presently signified by checking if the 'Color' of
-- the 'Brick' is the same as the 'Color' of the 'Ball' This should be changed
-- in the future.
--
-- There is no real concept of a player in bweakfwu. This should also be
-- introduced.
bang =
  World {
        paddles     = (Paddle (-35.0, 0) (1, 7) yellow 0 (False, False, False)
                      ,Paddle (35.0, 0) (1, 7) magenta 0 (False, False, False))
       ,balls       = (Ball (-34.0, 0) 0.5 yellow 0
                      ,Ball (34.0, 0) 0.5 magenta 0)
       ,board       = Board makeBoard
       ,scorekeeper = ScoreKeeper 0 0
       ,runningP    = True
      }

crunch :: World
-- | 'crunch' destroys a 'World'. Presently it makes a new 'World' as well. In
-- the future, it should only destroy a 'World'.
crunch = bang

view ::  World -> Picture
-- | 'view' redraws a 'World'.
view w =
  Scale worldScale worldScale
  $ Pictures [render . fst $ paddles w
             ,render . snd $ paddles w
             ,render . fst $ balls w
             ,render . snd $ balls w
             ,render $ board w
             ,render $ scorekeeper w
             ,Color white $ rectangleWire worldWidth worldHeight]
  where wh         = fromIntegral windowHeight
        ww         = fromIntegral windowWidth
        worldScale = min (wh / worldHeight) (ww / worldWidth)

step ::  StepTime -> World -> World
-- | 'step' checks if the game is in progress. If it is, it moves a 'World'
-- forward one step. If not, it stops a 'World'.
step t w =
  if gameP w
    then gameStep t w
    else gameStop

gameP ::  World -> Bool
-- | 'gameP' checks whether a game is in progress by checking if the 'Board'
-- is empty and if somebody is leading (which means there is a potential
-- winner).
gameP w =
  not (emptyBoard && winner)
  where emptyBoard = null . bricks $ board w
        winner = score1 sk /= score2 sk
        sk = scorekeeper w

gameStop ::  World
-- | 'gameStop' stops a 'World'.
gameStop = crunch

gameStep ::  StepTime -> World -> World
-- | 'gameStep' moves a 'World' forward one step.
gameStep t w =
  if runningP w
    then updateVisibles t
       $ updateTangibles t
       $ clampTangibles w
    else w

makeBoard ::  [Brick]
-- | 'board' makes a list of 'Brick's.
makeBoard = brickBoard 45 45

clampTangibles :: World -> World
-- | 'clampTangibles' makes sure the 'Tangible's of a 'World' do not move
-- off the edges of the 'World'.
clampTangibles (World p b bs s r) =
  let p'  = clampPaddles p
      b'  = clampBalls b
      bs' = bs
  in  World p' b' bs' s r

clampPaddles ::  (Paddle, Paddle) -> (Paddle, Paddle)
-- | 'clampPaddles' makes sure two 'Paddle's do not move off the edges of the
-- 'World'.
clampPaddles (p1, p2) = (clampPaddle p1, clampPaddle p2)

clampPaddle ::  Paddle -> Paddle
-- | 'clampPaddle' makes sure a 'Paddle' does not move off the edges of the
-- 'World'.
clampPaddle p@(Paddle (x, y) (w, h) c vv tv)
  | upTestBounds(y, hh)   = Paddle (x, hw - hh) (w, h) c (vv' (0, -1)) tv
  | downTestBounds(y, hh) = Paddle (x, -hw + hh) (w, h) c (vv' (0, 1)) tv
  | otherwise             = p
  -- 0.75 results in a small bump from the wall.
  -- A bigger number will give a bigger bump.
  where vv' n = reflect 0.75 n vv 0
        hh    = h/2
        hw    = worldHeight/2

clampBalls ::  (Ball, Ball) -> (Ball, Ball)
-- | 'clampBalls' makes sure two 'Ball's do not move off the edges of the
-- 'World'.
clampBalls (b1, b2) = (clampBall b1, clampBall b2)

clampBall ::  Ball -> Ball
-- | 'clampBall' makes sure a 'Ball' does not move off the edges of the
-- 'World'.
clampBall b@(Ball (x, y) r c (hv, vv))
  | upTestBounds (y, r)   = Ball (x, y) r c (hv, negate vv)
  | downTestBounds (y, r) = Ball (x, y) r c (hv, negate vv)
  | otherwise             = b

updateTangibles ::  StepTime -> World -> World
-- | 'updateTangibles' moves the 'Tangible's of a 'World' one step forward. It
-- reflects and moves all 'Movable's and updates the 'ScoreKeeper' as well.
updateTangibles t (World (p1, p2) (b1, b2) bs s r) =
  let p'                    = (move p1 t, move p2 t)
      ((b1', b2'), bs', s') = reflectBricks b1 b2 bs
      (b1'', b2'')          = uncurry reflectBalls (reflectPaddles b1' p1 p2
                                                   ,reflectPaddles b2' p1 p2)
      b'                    = (move b1'' t, move b2'' t)
  in  World p' b' bs' (mergeScores [s, s']) r

updateVisibles ::  StepTime -> World -> World
-- | 'updateVisibles' moves the visibles of a 'World' one step forward. It
-- updates the 'ScoreKeeper' of a 'World' and resets the 'Ball's if
-- necessary.
updateVisibles t (World p@(p1, p2) (b1, b2) bs s r) =
  let (newB1P, newB2P, s') = updateScores (b1, b2) s
      b1' = if newB1P then resetBall b1 p1 else b1
      b2' = if newB2P then resetBall b2 p2 else b2
      s'' = mergeScores [s', punish b1 t, punish b2 t]
      bs' = bs
  in  World p (b1', b2') bs' s'' r

updateScores ::  (Ball, Ball) -> ScoreKeeper -> (Bool, Bool, ScoreKeeper)
-- | 'updateScores' updates the 'ScoreKeeper'. It checks if either 'Ball' has
-- hit the x-edge of the 'World' and if so awards points and resets the
-- 'Ball'.
updateScores (b1, b2) s =
  (resetBall1, resetBall2, s')
  where result1    = updateScore b1
        result2    = updateScore b2
        resetBall1 = fst result1
        resetBall2 = fst result2
        s'         = mergeScores [s, snd result1, snd result2]

updateScore ::  Ball -> (Bool, ScoreKeeper)
-- | 'updateScore' updates a 'ScoreKeeper' It checks if the 'Ball' is out of
-- bounds. If the player's 'Ball' is at the opponent's edge of the 'World',
-- the player is awarded 50 points. If the player's 'Ball' is at the player's
-- edge, the player is deducted 30 points.
updateScore (Ball (x, _) r c _)
  | leftTestBounds (x, r/2)  = if c == yellow
                                 then (True, ScoreKeeper (-30) 0)
                                 else (True, ScoreKeeper 0 50)
  | rightTestBounds (x, r/2) = if c == yellow
                                 then (True, ScoreKeeper 50 0)
                                 else (True, ScoreKeeper 0 (-30))
  | otherwise                = (False, ScoreKeeper 0 0)

reflectPaddles ::  Ball -> Paddle -> Paddle -> Ball
-- | 'reflectPaddles' checks if a 'Ball' collides with any 'Paddle'. If there
-- is a collision, the 'Ball' is 'reflect'ed.
reflectPaddles b@(Ball p r c v) p1 p2 =
  maybe b new (test p1 <|> test p2)
  where test pad     = (\n -> (n, vel pad)) <$> collide pad b
        new (cn, cv) = Ball p r c (reflect 1.01 cn v cv)

reflectBricks ::  Ball -> Ball -> Board -> ((Ball, Ball), Board, ScoreKeeper)
-- | 'reflectBricks' checks if either 'Ball' collides with any of the 'Brick's
-- on the 'Board'. If a 'Ball' collides, it is 'reflect'ed and the 'Brick' it
-- hits has its 'Health' updated.
reflectBricks b1 b2 b  =
  ((b1', b2'), Board b2bricks, ScoreKeeper s1 s2)
  where (b1', b1bricks, s1) = reflectBricksWithBall b1 (bricks b)
        (b2', b2bricks, s2) = reflectBricksWithBall b2 b1bricks

reflectBricksWithBall ::  Ball -> [Brick] -> (Ball, [Brick], Score)
-- | 'reflectBricksWithBall' checks if a 'Ball' collides with any 'Brick' on
-- the 'Board'. If there is a collision, the 'Ball' is 'reflect'ed and the
-- 'Health' and ownership of the 'Brick' is updated.
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
    -- Award the responsible player with maxHealth^2 points.
    s                        = fromIntegral (sumH - sumH')^(2 :: Int)

reflectBrick ::  Ball -> Brick -> (Ball, Maybe Brick)
-- | 'reflectBrick' checks if a 'Ball' has collided with a 'Brick'. If there
-- is a collision, the 'Ball' is reflected and the 'Brick' is updated.
reflectBrick ball@(Ball p1 r c v) brick =
  maybe (ball, Just brick) new (collide brick ball)
  where new n = (Ball p1 r c (v' n), updateBrick brick ball (imp n))
        imp n = dvMag 1.01 n v
        v' n  = dvApply v n (imp n)

updateBrick ::  Brick -> Ball -> Float -> Maybe Brick
-- | 'updateBrick' updates the 'Health' and ownership of a 'Brick'. If the
-- 'Brick' reaches zero 'Health', it is removed from the 'Board'. If the
-- 'Brick' is neutral, it becomes owned by the player that hits it. If the
-- 'Brick' is owned by the player that hits it, it loses 'Health'. If it is
-- hit by the player which does not own it, it gains 'Health' up to its
-- possible 'MaxHealth'.
updateBrick brick@(Brick p s h maxH c) ball dvm =
  if h' > 0
    then Just (Brick p s h' maxH c')
    else Nothing
  where h'         = if c == white || c == colour ball
                       then downHealth
                       else upHealth
        downHealth = floor $ fromIntegral h - impact
        upHealth   = min (maxHealth brick) (ceiling $ fromIntegral h + impact)
        impact     = dvm / 50 -- Magic gameplay-suitable number.
        c'
          | c == white = colour ball
          | maxH == h' = white
          | otherwise  = c

reflectBalls ::  Ball -> Ball -> (Ball, Ball)
-- | 'reflectBalls' checks if two 'Ball's collide, and 'reflect' them if they
-- do.
reflectBalls b1@(Ball p1 r1 c1 v1) b2@(Ball p2 r2 c2 v2) =
  maybe (b1, b2) new (collide b1 b2)
    where new n = (Ball p1 r1 c1 (reflect 1.01 (negate n) v1 avVel)
                  ,Ball p2 r2 c2 (reflect 1.01 n v2 avVel))
          avVel = (v1 + v2) ^/^ 2

leftTestBounds ::  Point -> Bool
-- | 'leftTestBounds' tests if a 'Point' is within the left edge of the
-- 'World'
leftTestBounds (a, b) = a <= (-worldWidth/2) + b/2

rightTestBounds ::  Point -> Bool
-- | 'rightTestBounds' tests if a 'Point' is within the right edge of the
-- 'World'.
rightTestBounds (a, b) = a >= worldWidth/2 - b/2

upTestBounds ::  Point -> Bool
-- | 'upTestBounds' tests if a 'Point' is within the upper edge of the
-- 'World'.
upTestBounds (a, b) = a >= worldHeight/2 - b

downTestBounds ::  Point -> Bool
-- | 'downTestBounds' tests if a 'Point' is within the lower edge of the
-- 'World'.
downTestBounds (a, b) = a <= (-worldHeight/2) + b

resetBall ::  Ball -> Paddle -> Ball
-- | 'resetBall' resets a 'Ball' to the 'Paddle' that owns it.
resetBall (Ball _ r c _) p =
  Ball (x, 0) r c (0, 0)
  where x = if c == yellow
              then right p + 0.5
              else left p - 0.5

launchBall ::  Ball -> Paddle -> Ball
-- | 'launchBall' launches a 'Ball' in a direction that depends on the
-- position of its owner.
launchBall b@(Ball p r c v) paddle =
  case v of
    0 -> Ball p r c (negate (centre paddle)) -- Vel is based on paddle pos.
    _ -> b

notLaunched ::  Ball -> Bool
-- | 'notLaunched' checks if a 'Ball' is not yet launched from its 'Paddle'.
--
-- Presently we can check this simply by checking if the 'Ball' is still, as
-- this situation should only occur if a 'Ball' has not yet been launched.
notLaunched b = vel b == 0

punish ::  Ball -> StepTime -> ScoreKeeper
-- | 'punish' discourages bad sportmanship by subtracting points for bad
-- behaviour.
punish = punishIdling

punishIdling ::  Ball -> StepTime -> ScoreKeeper
-- | 'punishIdling' deducts points from a player's 'Score' if it has a not
-- launched its 'Ball'. This is to encourage the player to put the 'Ball' into
-- play as quickly as possible.
--
-- The main reason for this is to punish griefing in the form of waiting
-- until you have hit the opponent's 'Ball', and then launching your own 'Ball'
-- in the opposite direction.
punishIdling b t
  | notLaunched b = ScoreKeeper s1 s2
  | otherwise     = ScoreKeeper 0 0
  where (s1, s2)   = if colour b == yellow
                       then (punishment, 0)
                       else (0, punishment)
        punishment = negate (5 * t) -- Five points deducted per second.

worldWidth ::  Width
-- | 'worldWidth' is the 'Width' of the 'World'.
worldWidth = 80.0

worldHeight ::  Height
-- | 'worldHeight' is the 'Height' of the 'World'.
worldHeight = 45.0

updateRunning :: RunningP -> RunningP
-- | 'updateRunning' updates a 'RunningP' to dictate whether the world is
-- active or not.
updateRunning = not
