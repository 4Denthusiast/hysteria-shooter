module Automaton (
    CellState(..),
    GridState,
    WrapMode(..),
    editGrid,
    setGridCell,
    emptyGrid,
    stepGrid,
    stepMultiple
) where

import Data.List

data CellState = Alive | Dying1 | Dying2 | Dead deriving (Eq)

-- GridStates are additionally assumed to be rectangular.
type GridState = [[CellState]]

data WrapMode = Wrap | NoWrap

editList :: (a -> a) -> Int -> [a] -> [a]
editList f n xs = let (l, x:r) = splitAt n xs in l ++ (f x : r)

editGrid :: (CellState -> CellState) -> Int -> Int -> GridState -> GridState
editGrid f x y = editList (editList f x) y

setGridCell :: CellState -> Int -> Int -> GridState -> GridState
setGridCell = editGrid . const

emptyGrid :: Int -> Int -> GridState
emptyGrid x y = replicate y $ replicate x Dead

shiftLeft :: WrapMode -> GridState -> GridState
shiftLeft Wrap = map (\(c:cs) -> cs ++ [c])
shiftLeft NoWrap = map (\(_:cs) -> cs ++ [Dead])

shiftRight :: WrapMode -> GridState -> GridState
shiftRight Wrap = map (\cs -> last cs : init cs)
shiftRight NoWrap = map (\cs -> Dead : init cs)

shiftUp :: WrapMode -> GridState -> GridState
shiftUp Wrap (r:rs) = rs ++ [r]
shiftUp NoWrap (r:rs) = rs ++ [map (const Dead) r]

shiftDown :: WrapMode -> GridState -> GridState
shiftDown Wrap rs = last rs : init rs
shiftDown NoWrap rs = map (const Dead) (head rs) : init rs

stepCell :: [CellState] -> CellState
stepCell (self:neighbours) = case self of
        Alive -> if count == 3 || count == 4 || count == 5 then Alive else Dying1
        Dying1 -> Dying2
        Dying2 -> Dead
        Dead -> if count == 2 then Alive else Dead
    where count = length $ filter (Alive ==) neighbours

stepGrid :: WrapMode -> GridState -> GridState
stepGrid m g0 = map (map stepCell . transpose) $ transpose gs
    where gl = shiftLeft m g0
          gr = shiftRight m g0
          gs = [g0, shiftUp m gl, shiftUp m g0, shiftUp m gr, gl, gr, shiftDown m gl, shiftDown m g0, shiftDown m gr]

stepMultiple :: WrapMode -> Int -> GridState -> GridState
stepMultiple m 0 = id
stepMultiple m n = stepMultiple m (n-1) . stepGrid m
