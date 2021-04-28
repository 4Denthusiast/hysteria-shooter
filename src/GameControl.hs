module GameControl (
    registerKeyStateListeners,
    redrawCanvas,
    tick
) where

import Control.Monad.Trans
import qualified Data.Set as S
import Data.IORef
import Data.Text (pack)
import Graphics.UI.Gtk
import Prelude hiding (Left, Right)

import Game
import Display

type InputState = S.Set KeyVal

keyDownHandler :: IORef InputState -> EventM EKey Bool
keyDownHandler stateRef = do
    key <- eventKeyVal
    liftIO $ modifyIORef stateRef (S.insert key)
    return False

keyUpHandler :: IORef InputState -> EventM EKey Bool
keyUpHandler stateRef = do
    key <- eventKeyVal
    liftIO $ modifyIORef stateRef (S.delete key)
    return False

registerKeyStateListeners :: WidgetClass w => w -> IO (IORef InputState)
registerKeyStateListeners widget = do
    state <- newIORef S.empty
    on widget keyPressEvent (keyDownHandler state)
    on widget keyReleaseEvent (keyUpHandler state)
    return state

decodeInput :: InputState -> Input
decodeInput state =
         if keyDown "w" then Move Up
    else if keyDown "Up" then Move Up
    else if keyDown "a" then Move Left
    else if keyDown "Left" then Move Left
    else if keyDown "s" then Move Down
    else if keyDown "Down" then Move Down
    else if keyDown "d" then Move Right
    else if keyDown "Right" then Move Right
    else if keyDown "space" then Shoot
    else Noop
    where keyDown name = S.member (keyFromName (pack name)) state

redrawCanvas :: DrawingArea -> IORef GameState -> IO ()
redrawCanvas drawingArea gameRef = do
    drawWindow <- widgetGetDrawWindow drawingArea
    game <- readIORef gameRef
    (w, h) <- widgetGetSize drawingArea
    drawWindow <- widgetGetDrawWindow drawingArea
    drawWindowBeginPaintRect drawWindow (Rectangle 0 0 w h)
    drawGameState (toDrawable drawWindow) game
    drawWindowEndPaint drawWindow

tick :: IORef InputState -> IORef GameState -> DrawingArea -> IO ()
tick inputRef gameRef drawingArea = do
    input <- decodeInput <$> readIORef inputRef
    modifyIORef gameRef $ stepGame [input]
    redrawCanvas drawingArea gameRef
