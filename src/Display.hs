module Display (
    drawGameState,
    encodeColor,
    decodeColor
) where

import Data.List
import Data.Word
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (GC, gcNew, gcSetValues, newGCValues, foreground)
import Prelude hiding (Left, Right)

import Automaton
import Game

drawGameState :: Drawable -> GameState -> IO ()
drawGameState canvas (GameState _ goal grid players _) = do
    (csx, csy) <- drawableGetSize canvas
    gc <- gcNew canvas
    let drawCell x y c = drawGridCell gc canvas (csx,csy) (length (head grid), length grid) x y c
    let colorGrid = foldr drawPlayer (drawGoal goal $ map (map cellColor) grid) players
    mapM_ (\(row, y) -> mapM (\(s, x) -> drawCell x y s) (zip row [0..])) (zip colorGrid [0..])

drawGoal :: (Int, Int, Int, Int) -> [[Color]] -> [[Color]]
drawGoal (gx, gy, gWidth, gHeight) = editGrid gx gy (replicate gHeight $ replicate gWidth $ \c -> if c == cellColor Dead then encodeColor [0.2,0.4,0.8] else c)

drawPlayer :: PlayerState -> [[Color]] -> [[Color]]
drawPlayer p@PlayerState{playerX = x, playerY = y, facing = d, playerColor = c} = if isDead p then id else editGrid (x-2) (y-2) img
    where img = case d of
              Up -> img0
              Down -> reverse img0
              Left -> transpose img0
              Right -> transpose $ reverse img0
          img0 = [[id,c0,c0,id],
                  [c0,c1,c1,c0],
                  [c1,c2,c2,c1],
                  [c2,id,id,c2]]
          c0 = const $ encodeColor $ c
          c1 = const $ encodeColor $ map (*0.6) c
          c2 = const $ encodeColor $ map (*0.3) c

drawGridCell :: GC -> Drawable -> (Int, Int) -> (Int, Int) -> Int -> Int -> Color -> IO ()
drawGridCell gc canvas (csx, csy) (gsx, gsy) x y c = do
        gcSetValues gc (newGCValues{foreground = c})
        drawRectangle canvas gc True x0 y0 (x1 - x0) (y1 - y0)
    where x0 = div (x * csx) gsx
          x1 = div ((x+1) * csx) gsx
          y0 = div (y * csy) gsy
          y1 = div ((y+1) * csy) gsy

encodeColor :: [Float] -> Color
encodeColor [r, g, b] = Color (encode r) (encode g) (encode b)
    where encode x = round $ x * fromIntegral (maxBound :: Word16)

decodeColor :: Color -> [Float]
decodeColor (Color r g b) = map decode [r, g, b]
    where decode x = (fromInteger $ toInteger x) / (fromInteger $ toInteger (maxBound :: Word16))

outlineColor :: Color -> Color
outlineColor = encodeColor . map lighten . decodeColor
    where lighten x = min 1 (x + 0.15)

cellColor :: CellState -> Color
cellColor Alive  = encodeColor [1, 0, 0]
cellColor Dying1 = encodeColor [1, 0.5, 0]
cellColor Dying2 = encodeColor [1, 1, 0]
cellColor Dead   = encodeColor [0, 0, 0]
