module GameControl (
    InputState,
    registerKeyStateListeners,
    decodeInput,
    redrawCanvas,
    initialiseGame,
    keyDown,
    clearInputQueue,
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

data InputState = InputState [KeyVal] [KeyVal]

keyDownHandler :: IORef InputState -> EventM EKey Bool
keyDownHandler stateRef = do
    key <- eventKeyVal
    (InputState set queue) <- liftIO $ readIORef stateRef
    liftIO . writeIORef stateRef $ if elem key set then (InputState set queue) else (InputState (key:set) (queue ++ [key]))
    return False

keyUpHandler :: IORef InputState -> EventM EKey Bool
keyUpHandler stateRef = do
    key <- eventKeyVal
    (InputState set queue) <- liftIO $ readIORef stateRef
    liftIO $ writeIORef stateRef $ InputState (filter (key /=) set) queue
    return False

registerKeyStateListeners :: WidgetClass w => w -> IO (IORef InputState)
registerKeyStateListeners widget = do
    state <- newIORef (InputState [] [])
    on widget keyPressEvent (keyDownHandler state)
    on widget keyReleaseEvent (keyUpHandler state)
    return state

decodeInput :: InputState -> (Input, InputState)
decodeInput (InputState set queue) = (priority queueAction downAction, InputState set queue')
    where (queue', queueAction) = readQueue queue
          readQueue [] = ([], Noop)
          readQueue (k:ks) = case actionByKey k of {Noop -> readQueue ks; a -> (ks, a)}
          priority Noop a = a
          priority a _ = a
          downAction = foldr priority Noop $ map actionByKey set
          actionByKey 0x077 = Move Up -- w
          actionByKey 0x057 = Move Up -- W
          actionByKey 0xFF52 = Move Up -- Up
          actionByKey 0x061 = Move Left -- a
          actionByKey 0x041 = Move Left -- A
          actionByKey 0xFF51 = Move Left -- Left
          actionByKey 0x073 = Move Down -- s
          actionByKey 0x053 = Move Down -- S
          actionByKey 0xFF54 = Move Down -- Down
          actionByKey 0x064 = Move Right -- d
          actionByKey 0x044 = Move Right -- D
          actionByKey 0xFF53 = Move Right -- Right
          actionByKey 0x020 = Shoot -- space
          actionByKey 0xFF0D = Proceed -- enter
          actionByKey _ = Noop

clearInputQueue :: InputState -> InputState
clearInputQueue (InputState set _) = InputState set []

keyDown :: InputState -> String -> Bool
keyDown (InputState set queue) name = elem (keyFromName (pack name)) set

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

tick :: IORef InputState -> IORef GameState -> GameState -> DrawingArea -> Label -> IO ()
tick inputRef gameRef gameStart drawingArea healthLabel = do
    (input, inputState) <- decodeInput <$> readIORef inputRef
    writeIORef inputRef inputState
    modifyIORef gameRef $ stepGame [input]
    game <- readIORef gameRef
    let died = (\(GameState _ _ _ ps) -> any isDead ps) game
    if died && (input == Proceed) then writeIORef gameRef gameStart else return ()
    let [health] = (\(GameState _ _ _ ps) -> map playerHealth ps) game
    labelSetText healthLabel ("Player:\n"++ if health <= 0 then "Dead" else show health ++ "HP")
    redrawCanvas drawingArea gameRef
