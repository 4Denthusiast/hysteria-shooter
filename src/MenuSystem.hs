module MenuSystem (
    mainMenu,
) where

import Control.Monad.Trans
import Data.IORef
import Graphics.UI.Gtk

import Game
import MapLoading
import GameControl

mainMenu :: Window -> IORef InputState -> IO ()
mainMenu window inputRef = do
    box <- vBoxNew False 10
    containerAdd window box
    label <- labelNew $ Just "Hysteria Shooter"
    singleButton <- buttonNewWithLabel "Singleplayer"
    multiButton <- buttonNewWithLabel "Multiplayer"
    containerAdd box label
    containerAdd box singleButton
    containerAdd box multiButton
    onClicked singleButton $ selectLevel $ \game -> widgetDestroy box >> startSingleplayer window inputRef game
    return ()

displayMessage :: String -> IO ()
displayMessage message = do
    dialog <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk message
    dialogRun dialog
    widgetDestroy dialog

selectLevel :: (GameState -> IO ()) -> IO ()
selectLevel startGame = do
    fileDialog <- fileChooserDialogNew (Just "Open level file") Nothing FileChooserActionOpen [("Open", ResponseAccept), ("Cancel", ResponseCancel)]
    responseCode <- dialogRun fileDialog
    fp <- fileChooserGetFilename fileDialog
    widgetDestroy fileDialog
    case responseCode of
        ResponseDeleteEvent -> return ()
        ResponseCancel -> return ()
        ResponseAccept -> do
            level <- sequence $ loadLevel <$> fp
            case level of
                Nothing -> displayMessage "Corrupted level file."
                Just Nothing -> return ()
                Just (Just level') -> startGame level'

startSingleplayer :: Window -> IORef InputState -> GameState -> IO ()
startSingleplayer window keyboardState gameTemplate = do
    putStrLn "Starting singleplayer level."
    canvas <- drawingAreaNew
    gameState <- newIORef (initialiseGame gameTemplate [[0,1,0]])
    on canvas exposeEvent (liftIO $ redrawCanvas canvas gameState >> return False)
    timeoutAdd (tick keyboardState gameState canvas >> return True) 80
    containerAdd window canvas
    widgetShowAll window
