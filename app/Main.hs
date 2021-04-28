module Main where

import Control.Monad.Trans
import Data.IORef
import qualified Data.Set as S
import Data.Text (unpack)
import Graphics.UI.Gtk

import Lib
import GameControl

main :: IO ()
main = do
    initGUI
    window <- windowNew
    windowSetTitle window "Hysteria Shooter"
    windowSetDefaultSize window 512 512
    on window deleteEvent (liftIO mainQuit >> return False)
    keyboardState <- registerKeyStateListeners window
    canvas <- drawingAreaNew
    gameState <- newIORef testGame
    on canvas exposeEvent (liftIO $ redrawCanvas canvas gameState >> return False)
    timeoutAdd (tick keyboardState gameState canvas >> return True) 80
    containerAdd window canvas
    widgetShowAll window
    mainGUI
