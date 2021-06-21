module Main where

import Control.Monad.Trans
import Data.IORef
import qualified Data.Set as S
import Data.Text (unpack)
import Graphics.UI.Gtk

import Display
import GameControl
import MenuSystem

main :: IO ()
main = do
    initGUI
    window <- windowNew
    windowSetTitle window "Hysteria Shooter"
    windowSetDefaultSize window 512 512
    widgetModifyBg window StateNormal $ encodeColor [0.15, 0.15, 0.2]
    widgetModifyFg window StateNormal $ encodeColor [1, 1, 1]
    on window deleteEvent (liftIO mainQuit >> return False)
    keyboardState <- registerKeyStateListeners window
    mainMenu window keyboardState
    mainGUI
