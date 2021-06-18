{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Game (
    Direction(..),
    PlayerState(..),
    Input(..),
    GameState(..),
    stepGame,
    isDead
) where

import Data.Bytes.Serial
import GHC.Generics
import Prelude hiding (Left, Right)

import Automaton

data Direction = Up | Down | Left | Right deriving (Eq, Generic, Serial, Show)

data PlayerState = PlayerState {
    playerX :: Int,
    playerY :: Int,
    facing :: Direction,
    reloadTime :: Int,
    playerHealth :: Int,
    playerColor :: [Float]
} deriving (Eq, Generic, Serial, Show)

data Input = Noop | Shoot | Proceed | Move Direction deriving (Eq, Generic, Serial, Show)

data GameState = GameState WrapMode (Int, Int, Int, Int) GridState [PlayerState] deriving (Eq, Generic, Serial, Show)

stepGame :: [Input] -> GameState -> GameState
stepGame inputs (GameState mode goal grid players) = GameState mode goal grid' players'
    where grid' = stepGrid mode $ foldr addShot grid (zip inputs players)
          players' = zipWith (stepPlayer grid') inputs players

directionX, directionY :: Direction -> Int
directionX Left = -1
directionX Right = 1
directionX _ = 0
directionY Up = -1
directionY Down = 1
directionY _ = 0

isDead :: PlayerState -> Bool
isDead = (<= 0) . playerHealth

addShot :: (Input, PlayerState) -> GridState -> GridState
addShot (Shoot, p@PlayerState{playerX = px, playerY = py, facing = d, reloadTime = 0}) grid | not (isDead p) =
    setGridCells (px + directionX d - 1) (py + directionY d - 1) (case d of
        Up -> [[Alive, Alive], [Dying1, Dying1]]
        Down -> [[Dying1, Dying1], [Alive, Alive]]
        Left -> [[Alive, Dying1], [Alive, Dying1]]
        Right -> [[Dying1, Alive], [Dying1, Alive]]
    ) grid
addShot _ grid = grid

stepPlayer :: GridState -> Input -> PlayerState -> PlayerState
stepPlayer grid input p = if isDead p then p else move $ hurt $ recharge p
    where recharge player = player{reloadTime = if reloadTime player > 0 then reloadTime player - 1 else if input == Shoot then 10 else 0}
          move player@PlayerState{facing = prevDirection} = case input of
              Move newDirection -> if newDirection /= prevDirection then player{facing = newDirection} else if
                      (aliveCount grid (aheadRectangle player) == 0) && case prevDirection of
                          Up -> playerY player > 2
                          Down -> playerY player < length grid - 2
                          Left -> playerX player > 2
                          Right -> playerX player < length (head grid) - 2
                  then player{playerX = playerX player + directionX prevDirection, playerY = playerY player + directionY prevDirection}
                  else player
              _ -> player
          hurt player@PlayerState{playerX = px, playerY = py} = player{playerHealth = playerHealth player - aliveCount grid (px-2, py-2, 4, 4)}
          aheadRectangle player@PlayerState{playerX = x, playerY = y, facing = d} = (x + directionX d - 2 + max 0 (directionX d) * 3, y + directionY d - 2 + max 0 (directionY d) * 3, 1 + 3 * abs (directionY d), 1 + 3 * abs (directionX d))

aliveCount :: GridState -> (Int, Int, Int, Int) -> Int
aliveCount grid (x, y, w, h) = length $ filter (Alive ==) $ concatMap (take w . drop x) $ take h $ drop y $ grid
