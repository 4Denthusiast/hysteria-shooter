module MapLoading (
    loadLevel
) where

import Data.List
import Text.Read

import Automaton
import Game

loadLevel :: FilePath -> IO (Maybe GameState)
loadLevel fp = (parseLines . lines) <$> readFile fp

parseLines :: [String] -> Maybe GameState
parseLines (width:height:px:py:health:gx:gy:gWidth:gHeight:wrap:gridData) = do
    width' <- readMaybe width
    height' <- readMaybe height
    px' <- readMaybe px
    py' <- readMaybe py
    health' <- readMaybe health
    gx' <- readMaybe gx
    gy' <- readMaybe gy
    gWidth' <- readMaybe gWidth
    gHeight' <- readMaybe gHeight
    wrap' <- case wrap of
        "0" -> Just NoWrap
        "1" -> Just Wrap
        _ -> Nothing
    grid <- splitInto width' height' =<< mapM parseCell (concat gridData)
    return $ GameState wrap' (gx', gy', gWidth', gHeight') grid [PlayerState px' py' Up 0 health' [1,1,1]] Unpaused

parseCell :: Char -> Maybe CellState
parseCell '0' = Just Dead
parseCell '1' = Just Dying2
parseCell '2' = Just Dying1
parseCell '3' = Just Alive
parseCell '4' = Just Dead --Used in the original for markings

splitInto :: Int -> Int -> [a] -> Maybe [[a]]
splitInto w 0 [] = Just []
splitInto w 0 xs = Nothing
splitInto w h xs = let (r, xs') = splitAt w xs in if null xs' && length r < w then Nothing else (r:) <$> splitInto w (h - 1) xs'
