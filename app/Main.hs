module Main where

import Control.Monad.Trans
import Graphics.UI.Gtk

import Lib

main :: IO ()
main = do
    initGUI
    window <- windowNew
    windowSetTitle window "Hysteria Shooter"
    windowSetDefaultSize window 512 512
    on window deleteEvent (liftIO mainQuit >> return False)
    widgetShowAll window
    mainGUI
