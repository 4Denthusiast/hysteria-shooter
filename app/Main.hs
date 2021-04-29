module Main where

import Control.Monad.Trans
import Data.IORef
import qualified Data.Set as S
import Data.Text (unpack)
import Graphics.UI.Gtk

import GameControl
import MenuSystem

main :: IO ()
main = do
    initGUI
    window <- windowNew
    windowSetTitle window "Hysteria Shooter"
    windowSetDefaultSize window 512 512
    on window deleteEvent (liftIO mainQuit >> return False)
    keyboardState <- registerKeyStateListeners window
    mainMenu window keyboardState
    widgetShowAll window
    mainGUI
