{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Automaton (
    CellState(..),
    GridState,
    WrapMode(..),
    editGrid,
    setGridCells,
    emptyGrid,
    stepGrid,
    stepMultiple
) where

import Data.Bytes.Serial
import Data.List
import GHC.Generics

data CellState = Alive | Dying1 | Dying2 | Dead deriving (Eq, Generic, Serial, Show)

-- GridStates are additionally assumed to be rectangular.
type GridState = [[CellState]]

data WrapMode = Wrap | NoWrap deriving (Eq, Generic, Serial, Show)

editList :: Int -> [a -> a] -> [a] -> [a]
editList n fs xs = let (l, r) = splitAt n xs in l ++ zipWith id (fs ++ repeat id) r

editGrid :: Int -> Int -> [[a -> a]] -> [[a]] -> [[a]]
editGrid x y fss = editList y (map (editList x) fss)

setGridCells :: Int -> Int -> [[a]] -> [[a]] -> [[a]]
setGridCells x y = editGrid x y . map (map const)

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
