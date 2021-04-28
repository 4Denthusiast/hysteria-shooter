module Game (
    Direction(..),
    PlayerState(..),
    Input(..),
    GameState(..),
    stepGame
) where

import Automaton

import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right deriving (Eq)

data PlayerState = PlayerState {
    playerX :: Int,
    playerY :: Int,
    facing :: Direction,
    reloadTime :: Int,
    playerHealth :: Int,
    playerColor :: [Float]
}

data Input = Noop | Shoot | Move Direction deriving (Eq)

data GameState = GameState WrapMode GridState [PlayerState]

stepGame :: [Input] -> GameState -> GameState
stepGame inputs (GameState mode grid players) = GameState mode grid' players'
    where grid' = foldr addShot (stepGrid mode grid) (zip inputs players)
          players' = zipWith (stepPlayer grid') inputs players

directionX, directionY :: Direction -> Int
directionX Left = -1
directionX Right = 1
directionX _ = 0
directionY Up = -1
directionY Down = 1
directionY _ = 0

addShot :: (Input, PlayerState) -> GridState -> GridState
addShot (Shoot, PlayerState{playerX = px, playerY = py, facing = d, reloadTime = 0}) grid =
    setGridCell Alive (px + min 2 (directionX d * 3)) (py + min 2 (directionY d * 3)) $
    setGridCell Alive (px + min 2 (directionX d * 3) - abs (directionY d)) (py + min 2 (directionY d * 3) - abs (directionX d)) $
    setGridCell Dying1 (px + min 1 (directionX d * 2)) (py + min 1 (directionY d * 2)) $
    setGridCell Dying1 (px + min 1 (directionX d * 2) - abs (directionY d)) (py + min 1 (directionY d * 2) - abs (directionX d)) $
    grid
addShot _ grid = grid

stepPlayer :: GridState -> Input -> PlayerState -> PlayerState
stepPlayer grid input = hurt . move . recharge
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
          aheadRectangle player@PlayerState{playerX = x, playerY = y, facing = d} = (x + directionX d - 2 + max 0 (directionX d) * 4, y + directionY d - 2 + max 0 (directionY d) * 4, 1 + 3 * abs (directionY d), 1 + 3 * abs (directionX d))

aliveCount :: GridState -> (Int, Int, Int, Int) -> Int
aliveCount grid (x, y, w, h) = length $ filter (Alive ==) $ concatMap (take w . drop x) $ take h $ drop y $ grid
