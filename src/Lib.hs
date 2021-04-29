module Lib (
    testGame
) where

import Control.Monad.Trans
import Data.IORef
import Graphics.UI.Gtk
import Prelude hiding (Left, Right)

import Automaton
import Game
import Display

testGame :: GameState
testGame = GameState Wrap (emptyGrid 99 99) [PlayerState 4 4 Right 0 20 [0,1,0]]
