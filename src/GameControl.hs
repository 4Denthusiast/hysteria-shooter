module GameControl (
    InputState,
    registerKeyStateListeners,
    redrawCanvas,
    initialiseGame,
    keyDown,
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
         if keyDown state "w" then Move Up
    else if keyDown state "Up" then Move Up
    else if keyDown state "a" then Move Left
    else if keyDown state "Left" then Move Left
    else if keyDown state "s" then Move Down
    else if keyDown state "Down" then Move Down
    else if keyDown state "d" then Move Right
    else if keyDown state "Right" then Move Right
    else if keyDown state "space" then Shoot
    else Noop

keyDown :: InputState -> String -> Bool
keyDown state name = S.member (keyFromName (pack name)) state

redrawCanvas :: DrawingArea -> IORef GameState -> IO ()
redrawCanvas drawingArea gameRef = do
    drawWindow <- widgetGetDrawWindow drawingArea
    game <- readIORef gameRef
    (w, h) <- widgetGetSize drawingArea
    drawWindow <- widgetGetDrawWindow drawingArea
    drawWindowBeginPaintRect drawWindow (Rectangle 0 0 w h)
    drawGameState (toDrawable drawWindow) game
    drawWindowEndPaint drawWindow

initialiseGame :: GameState -> [[Float]] -> GameState
initialiseGame (GameState mode goal grid [player]) colors = GameState mode goal grid $ map (\c -> player{playerColor = c}) colors

tick :: IORef InputState -> IORef GameState -> GameState -> DrawingArea -> IO ()
tick inputRef gameRef gameStart drawingArea = do
    input <- decodeInput <$> readIORef inputRef
    modifyIORef gameRef $ stepGame [input]
    died <- (\(GameState _ _ _ ps) -> any isDead ps) <$> readIORef gameRef
    enter <- flip keyDown "Return" <$> readIORef inputRef
    if died && enter then writeIORef gameRef gameStart else return ()
    redrawCanvas drawingArea gameRef
