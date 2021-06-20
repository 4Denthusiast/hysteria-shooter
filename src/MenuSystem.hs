module MenuSystem (
    mainMenu,
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Function (fix)
import Data.IORef
import Data.List
import Data.Text (Text)
import Graphics.UI.Gtk hiding (Socket)
import Network.Simple.TCP
import Numeric (showHex)
import Text.Read
import System.Directory
import System.FilePath

import Game
import MapLoading
import Display
import GameControl
import Multiplayer

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
    label <- labelNewWithMarkup "<span foreground=\"#ff2000\" weight=\"bold\" size=\"x-large\">Hysteria Shooter</span>"
    singleButton <- buttonNewWithLabel "Singleplayer"
    multiButton <- buttonNewWithLabel "Multiplayer"
    containerAdd box label
    containerAdd box singleButton
    containerAdd box multiButton
    onClicked singleButton $ selectLevel $ \levels -> startSingleplayer window inputRef levels
    onClicked multiButton $ serverConnectScreen window inputRef
    widgetShowAll window

labelNewWithMarkup :: String -> IO Label
labelNewWithMarkup s = do
    label <- labelNew (Nothing :: Maybe String)
    labelSetMarkup label s
    return label

displayMessage :: String -> IO ()
displayMessage message = do
    dialog <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk message
    dialogRun dialog
    widgetDestroy dialog

selectLevel :: ([GameState] -> IO ()) -> IO ()
selectLevel startGame = do
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
            case sequence levels of
                Nothing -> displayMessage "Corrupted level file."
                Just levels' -> startGame levels'

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

startSingleplayer :: Window -> IORef InputState -> [GameState] -> IO ()
startSingleplayer window keyboardState [] = displayMessage "You won." >> mainMenu window keyboardState
startSingleplayer window keyboardState (gameTemplate:nextLevels) = do
    clearWindow window
    box <- vBoxNew False 0
    containerAdd window box
    healthLabel <- labelNew (Nothing :: Maybe Text)
    boxPackStart box healthLabel PackNatural 5
    widgetModifyFg healthLabel StateNormal (encodeColor [0,1,0])
    aspectFrame <- aspectFrameNew 0.5 0.5 (Just 1)
    frameSetShadowType aspectFrame ShadowNone
    canvas <- drawingAreaNew
    containerAdd box aspectFrame
    containerAdd aspectFrame canvas
    let gameStart = initialiseGame gameTemplate [[0,1,0]]
    gameState <- newIORef gameStart
    modifyIORef keyboardState clearInputQueue
    on canvas exposeEvent (liftIO $ redrawCanvas canvas gameState >> return False)
    timeoutAdd (tick keyboardState gameState gameStart canvas healthLabel >> (not <$> checkWon keyboardState gameState (startSingleplayer window keyboardState nextLevels))) 60
    widgetShowAll window

checkWon :: IORef InputState -> IORef GameState -> IO () -> IO Bool
checkWon keyboardState gameState nextLevel = do
        enter <- flip keyDown "Return" <$> readIORef keyboardState
        won <- isWonState <$> readIORef gameState
        if (enter && won) then nextLevel else return ()
        return (enter && won)

serverConnectScreen :: Window -> IORef InputState -> IO ()
serverConnectScreen window inputRef = do
    clearWindow window
    box <- vBoxNew False 5
    containerAdd window box
    containerAdd box =<< labelNewWithMarkup "<span foreground=\"white\">Server hostname:</span>"
    hostNameInput <- entryNew
    entrySetText hostNameInput "localhost"
    containerAdd box hostNameInput
    containerAdd box =<< labelNewWithMarkup "<span foreground=\"white\">Server port:</span>"
    hostPortInput <- entryNew
    entrySetText hostPortInput "54503"
    containerAdd box hostPortInput
    containerAdd box =<< labelNewWithMarkup "<span foreground=\"white\">Your colour:</span>"
    colorInput <- colorButtonNew
    colorButtonSetColor colorInput (encodeColor [0,1,0])
    containerAdd box colorInput
    connectButton <- buttonNewWithLabel "Connect"
    containerAdd box connectButton
    onClicked connectButton $ connectToServer window inputRef hostNameInput hostPortInput colorInput
    widgetShowAll window

connectToServer :: Window -> IORef InputState -> Entry -> Entry -> ColorButton -> IO ()
connectToServer window inputRef hostNameInput hostPortInput colorButton = do
    hostName <- entryGetText hostNameInput
    hostPort <- entryGetText hostPortInput
    color <- decodeColor <$> colorButtonGetColor colorButton
    (sock, remoteAddr) <- connectSock hostName hostPort
    sendNetMessage sock $ PlayerConnected color
    response <- recvNetMessage sock
    case response of
        Just ServerIsBusy -> displayMessage "The server is already busy running a game. Try again later." >> closeSock sock
        Just (YourIdIs id) -> awaitOtherPlayers window inputRef sock id
        _ -> displayMessage "Unknown server connection error." >> mainMenu window inputRef >> closeSock sock

awaitOtherPlayers :: Window -> IORef InputState -> Socket -> Int -> IO ()
awaitOtherPlayers window inputRef sock id = do
        clearWindow window
        box <- vBoxNew False 5
        containerAdd window box
        label <- labelNew (Nothing :: Maybe String)
        containerAdd box label
        button <- buttonNewWithLabel "Select Level"
        containerAdd box button
        remainingLevels <- newIORef Nothing
        onClicked button $ selectLevel $ \levels -> writeIORef remainingLevels (Just levels) >> sendNetMessage sock (StartLevel 0 (head levels))
        forkIO $ fix (\loop colors -> do
                messageM <- recvNetMessage sock
                case messageM of
                    Nothing -> postGUISync $ displayMessage "Disconnected from server." >> mainMenu window inputRef >> closeSock sock
                    Just (PlayerConnected color) -> (postGUISync $ labelSetMarkup label (playerList (colors ++ [color])) >> widgetShowAll window) >> loop (colors ++ [color])
                    Just (StartLevel 0 level) -> postGUIAsync $ startMultiplayer window inputRef sock colors id level =<< readIORef remainingLevels
                    _ -> postGUISync $ displayMessage "Error: Unexpected network message." >> mainMenu window inputRef >> closeSock sock
            ) []
        widgetShowAll window
    where playerList colors = intercalate "\n" $ zipWith (\id' [r,g,b] -> "<span foreground=\"#" ++ hexColor r ++ hexColor g ++ hexColor b ++ (if id' == id then "\" underline=\"single\">Player " else "\">Player ") ++ show id' ++ "</span>") [0..] colors
          hexColor x = reverse $ take 2 $ reverse $ "0" ++ showHex (round (x * 255)) ""

startMultiplayer :: Window -> IORef InputState -> Socket -> [[Float]] -> Int -> GameState -> Maybe [GameState] -> IO ()
startMultiplayer window inputRef sock colors id level levels = do
    clearWindow window
    box <- vBoxNew False 0
    containerAdd window box
    healthBox <- hBoxNew False 0
    boxPackStart box healthBox PackNatural 5
    healthLabels <- forM (zip colors [0..]) $ \(color, id') -> do
        healthLabel <- labelNew (Nothing :: Maybe Text)
        containerAdd healthBox healthLabel
        widgetModifyFg healthLabel StateNormal (encodeColor color)
        labelSetUseUnderline healthLabel (id == id')
        return healthLabel
    aspectFrame <- aspectFrameNew 0.5 0.5 (Just 1)
    frameSetShadowType aspectFrame ShadowNone
    canvas <- drawingAreaNew
    containerAdd box aspectFrame
    containerAdd aspectFrame canvas
    stateVar <- newMVar =<< initialMultiplayerState level colors
    gameStateRef <- newIORef level -- Technically this should be the initialised state, but it barely matters.
    on canvas exposeEvent (liftIO $ redrawCanvas canvas gameStateRef >> return False)
    forkIO $ fix (\loop -> do
            messageM <- recvNetMessage sock
            case messageM of
                Nothing -> closeSock sock
                Just message -> modifyMVar_ stateVar (return . updateStateWithMessage id message) >> loop
        )
    multiplayerTick window (\s -> displayMessage s >> mainMenu window inputRef) inputRef sock stateVar id gameStateRef canvas healthLabels levels
    return ()
