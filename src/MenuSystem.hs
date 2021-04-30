module MenuSystem (
    mainMenu,
) where

import Control.Monad.Trans
import Data.Char
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import Text.Read
import System.Directory
import System.FilePath

import Game
import MapLoading
import GameControl

clearWindow :: Window -> IO ()
clearWindow window = do
    child <- binGetChild window
    case child of
        Nothing -> return ()
        Just c -> widgetDestroy c

mainMenu :: Window -> IORef InputState -> IO ()
mainMenu window inputRef = do
    clearWindow window
    box <- vBoxNew False 10
    containerAdd window box
    label <- labelNew $ Just "Hysteria Shooter"
    widgetModifyFg window StateNormal (Color maxBound 0 0)
    singleButton <- buttonNewWithLabel "Singleplayer"
    multiButton <- buttonNewWithLabel "Multiplayer"
    containerAdd box label
    containerAdd box singleButton
    containerAdd box multiButton
    onClicked singleButton $ selectLevel window inputRef $ \game next -> startSingleplayer window inputRef game next
    widgetShowAll window

displayMessage :: String -> IO ()
displayMessage message = do
    dialog <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk message
    dialogRun dialog
    widgetDestroy dialog

selectLevel :: Window -> IORef InputState -> (GameState -> IO () -> IO ()) -> IO ()
selectLevel window inputRef startGame = do
    fileDialog <- fileChooserDialogNew (Just "Open level file") Nothing FileChooserActionOpen [("Open", ResponseAccept), ("Cancel", ResponseCancel)]
    fileChooserSetFilename fileDialog =<< getCurrentDirectory
    responseCode <- dialogRun fileDialog
    fp' <- fileChooserGetFilename fileDialog
    widgetDestroy fileDialog
    case (responseCode, fp') of
        (ResponseDeleteEvent, _) -> return ()
        (ResponseCancel, _) -> return ()
        (ResponseAccept, Nothing) -> return ()
        (ResponseAccept, Just fp) -> do
            fps <- getLevelSequence fp
            levels <- mapM loadLevel fps
            foldr (\level nextLevel -> case level of
                    Nothing -> displayMessage "Corrupted level file."
                    Just level' -> startGame level' nextLevel
                ) (displayMessage "You won." >> mainMenu window inputRef) levels

getLevelSequence :: FilePath -> IO [FilePath]
getLevelSequence fp0 = takeWhileExists paths
    where takeWhileExists [] = return []
          takeWhileExists (fp:fps) = (\e -> if e then (fp:) <$> takeWhileExists fps else return []) =<< doesFileExist fp
          (fp1,ext) = splitExtension fp0
          fp2 = dropWhileEnd isDigit fp1
          n = (readMaybe $ reverse $ takeWhile isDigit $ reverse fp1) :: Maybe Int
          paths = case n of
              Nothing -> [fp0]
              Just n0 -> map (\n' -> addExtension (fp2 ++ show n') ext) [n0..]

startSingleplayer :: Window -> IORef InputState -> GameState -> IO () -> IO ()
startSingleplayer window keyboardState gameTemplate nextLevel = do
    clearWindow window
    box <- vBoxNew False 0
    aspectFrame <- aspectFrameNew 0.5 0.5 (Just 1)
    frameSetShadowType aspectFrame ShadowNone
    canvas <- drawingAreaNew
    containerAdd window box
    containerAdd box aspectFrame
    containerAdd aspectFrame canvas
    let gameStart = initialiseGame gameTemplate [[0,1,0]]
    gameState <- newIORef gameStart
    on canvas exposeEvent (liftIO $ redrawCanvas canvas gameState >> return False)
    timeoutAdd (tick keyboardState gameState gameStart canvas >> (not <$> checkWon keyboardState gameState nextLevel)) 80
    widgetShowAll window

checkWon :: IORef InputState -> IORef GameState -> IO () -> IO Bool
checkWon keyboardState gameState nextLevel = do
        enter <- flip keyDown "Return" <$> readIORef keyboardState
        won <- (\(GameState _ goal _ ps) -> all (playerWon goal) ps) <$> readIORef gameState
        if (enter && won) then nextLevel else return ()
        return (enter && won)
    where playerWon (gx, gy, gw, gh) PlayerState{playerX=x, playerY=y, playerHealth=h} = h > 0 && x >= gx + 2 && y >= gy + 2 && x <= gx + gw - 2 && y <= gy + gh - 2
