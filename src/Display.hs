module Display (
    drawGameState
) where

import Data.Word
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (gcNewWithValues, gcSetValues, newGCValues, foreground)

import Automaton
import Game

drawGameState :: Drawable -> GameState -> IO ()
drawGameState canvas (GameState _ grid _) = do
    (csx, csy) <- drawableGetSize canvas
    let drawCell x y s = drawGridCell canvas (csx,csy) (length (head grid), length grid) x y (cellColor s)
    mapM_ (\(row, y) -> mapM (\(s, x) -> drawCell x y s) (zip row [0..])) (zip grid [0..])

drawGridCell :: Drawable -> (Int, Int) -> (Int, Int) -> Int -> Int -> Color -> IO ()
drawGridCell canvas (csx, csy) (gsx, gsy) x y c = do
        gc <- gcNewWithValues canvas (newGCValues{foreground = c})
        drawRectangle canvas gc True x0 y0 (x1 - x0 - 1) (y1 - y0 - 1)
        gcSetValues gc (newGCValues{foreground = outlineColor c})
        drawLines canvas gc [(x0,y1-1), (x1-1,y1-1), (x1-1,y0)]
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
    where lighten x = min 1 (x + 0.3)

cellColor :: CellState -> Color
cellColor Alive  = encodeColor [1, 0, 0]
cellColor Dying1 = encodeColor [1, 0.5, 0]
cellColor Dying2 = encodeColor [1, 1, 0]
cellColor Dead   = encodeColor [0, 0, 0]
