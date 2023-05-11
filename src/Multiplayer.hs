{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Multiplayer (
    NetMessage(..),
    sendNetMessage,
    recvNetMessage,
    initialMultiplayerState,
    updateStateWithMessage,
    multiplayerTick
) where

import Control.Concurrent.MVar
import Control.Exception (handle, throwIO)
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Either.Extra
import Data.IORef
import Data.List
import Data.Maybe
import GHC.Generics
import Graphics.UI.Gtk hiding (Socket)
import Network.Simple.TCP
import System.Clock
import System.IO.Error

import Game
import GameControl

data NetMessage = PlayerConnected [Float] | YourIdIs Int | ServerIsBusy | Disconnect | StartLevel Int GameState | InputFrom Int Input | TimePing Integer deriving (Eq, Generic, Serial, Show)

sendNetMessage :: Socket -> NetMessage -> IO ()
sendNetMessage sock msg = sendWithLength $ runPutS $ serialize msg
    where sendWithLength bs = sendLazy sock $ BSL.fromChunks [bsLen bs, bs]
          bsLen bs = runPutS $ putWord32be $ fromInteger $ toInteger $ BS.length bs

recvNetMessage :: Socket -> IO (Maybe NetMessage)
recvNetMessage sock = runMaybeT $ do
        lengthBs <- recvAll 4
        lengthWord <- MaybeT $ return $ eitherToMaybe $ runGetS getWord32be lengthBs
        let length = fromInteger $ toInteger $ lengthWord
        messageBS <- recvAll length
        MaybeT $ return $ eitherToMaybe $ runGetS deserialize messageBS
    where recvAll 0 = return BS.empty
          recvAll n = do
              first <- MaybeT $ handle (\e -> if isResourceVanishedError e then return Nothing else throwIO e) $ recv sock n
              rest <- recvAll (n - BS.length first)
              return $ BS.append first rest

-- The number of ticks after a level change is triggered to actually start running it.
levelChangeWait :: Int
levelChangeWait = 10

data MultiplayerState = MultiplayerState {
    multiplayerStartTime :: TimeSpec,
    multiplayerReferenceState :: GameState,
    multiplayerReferenceTick :: Int,
    multiplayerCurrentTick :: Int,
    multiplayerPendingLevel :: Maybe (Int, GameState),
    multiplayerInputs :: [[Input]]
}

initialMultiplayerState :: GameState -> [[Float]] -> IO MultiplayerState
initialMultiplayerState level players = do
    time <- getTime Monotonic
    return $ MultiplayerState time (initialiseGame level players) 0 0 (Just (levelChangeWait, level)) (map (const [Noop]) players)

addInput :: Input -> Int -> [[Input]] -> [[Input]]
addInput i id iss = let (issL, is:issR) = splitAt id iss in issL ++ (is ++ [i]) : issR

updateStateWithMessage :: Int -> NetMessage -> MultiplayerState -> IO MultiplayerState
updateStateWithMessage id (InputFrom id' act) state = return $ if id == id' then state else state{multiplayerInputs = addInput act id' $ multiplayerInputs state}
updateStateWithMessage id (StartLevel tick level) state = return $ state{multiplayerPendingLevel = Just (tick, level)}
updateStateWithMessage id (TimePing rt) state@MultiplayerState{multiplayerStartTime = st} = do
    ct <- getTime Monotonic
    --putStrLn ("Received recommended time "++show (fromNanoSecs rt)++", current time = "++show (ct - st)++". Adjusting to "++show (ct - st + fromNanoSecs (div (rt - toNanoSecs (ct - st)) 100)))
    return $ state{multiplayerStartTime = st - fromNanoSecs (div (rt - toNanoSecs (ct - st)) 100)}
updateStateWithMessage _ m _ = error ("Unexpected net message: " ++ show m)

guessInputs :: [Input] -> [Input]
guessInputs (x0:x1:xs) = x0 : guessInputs (x1:xs)
guessInputs [x] = repeat x
guessInputs [] = error "Empty input queue."

multiplayerTick :: Window -> (String -> IO ()) -> IORef InputState -> Socket -> MVar MultiplayerState -> Int -> IORef GameState -> DrawingArea -> [Label] -> Maybe [GameState] -> IO Bool
multiplayerTick window end inputRef sock stateVar id gameStateRef drawingArea healthLabels remainingLevels = do
    (input, inputState) <- decodeInput <$> readIORef inputRef
    writeIORef inputRef inputState
    sendNetMessage sock $ InputFrom id input
    MultiplayerState startTime referenceState referenceTick currentTick pendingLevel inputs <- takeMVar stateVar
    let inputs' = addInput input id inputs
    let definiteInputs = takeWhile ((== length inputs) . length) $ tail $ transpose inputs'
    let definiteInputs' = case pendingLevel of
            Nothing -> definiteInputs
            Just (changeTick, _) -> take (changeTick - referenceTick) definiteInputs
    let referenceState' = case pendingLevel of
            Nothing -> foldl (flip stepGame) referenceState definiteInputs
            Just (_, newLevel) -> initialiseGame newLevel (map playerColor $ (\(GameState _ _ _ ps _) -> ps) referenceState)
    let referenceTick' = referenceTick + length definiteInputs'
    let currentTick' = currentTick + 1
    let guessedInputs = take (currentTick + 1 - referenceTick') $ drop (length definiteInputs' + 1) $ transpose (map guessInputs inputs')
    let guessedGameState = foldl (flip stepGame) referenceState' guessedInputs
    writeIORef gameStateRef guessedGameState
    let (GameState _ _ _ predictedPlayerStates _) = guessedGameState
    forM (zip3 healthLabels predictedPlayerStates [0..]) $ \(label, PlayerState _ _ _ _ health _, id') -> labelSetText label ("P"++show id'++":\n"++ if health <= 0 then "Dead" else show health ++ "HP")
    let inputs'' = map (drop (length definiteInputs')) inputs'
    let (newPendingLevel, remainingLevels') = if remainingLevels /= Nothing && pendingLevel == Nothing && input == Proceed
            then if (\(GameState _ _ _ ps _) -> any isDead ps) referenceState'
                then (Just $ head <$> remainingLevels, remainingLevels)
                else if isWonState referenceState'
                    then ((listToMaybe . tail) <$> remainingLevels, tail <$> remainingLevels)
                    else (Nothing, remainingLevels)
            else (Nothing, remainingLevels)
    pendingLevel' <- case newPendingLevel of
        Just (Just newPending) -> sendNetMessage sock (StartLevel (currentTick + levelChangeWait) newPending) >> return (Just (currentTick + levelChangeWait, newPending))
        _ -> return $ if pendingLevel /= Nothing && fst (fromJust pendingLevel) <= referenceTick' then Nothing else pendingLevel
    putMVar stateVar $ MultiplayerState startTime referenceState' referenceTick' currentTick' pendingLevel' inputs''
    redrawCanvas drawingArea gameStateRef
    --widgetShowAll window
    time <- getTime Monotonic
    let nextTickTime = fromNanoSecs (fromIntegral $ currentTick' * div (10^9) 15) + startTime
    let delay = max 1 $ fromInteger $ div (toNanoSecs (nextTickTime - time)) 1000000
    sendNetMessage sock $ TimePing $ toNanoSecs $ time - startTime
    if currentTick' - referenceTick' > 30
        then closeSock sock >> end "Falling too far behind, aborting."
        else if newPendingLevel == Just Nothing
            then closeSock sock >> end "You won!"
            else timeoutAdd (multiplayerTick window end inputRef sock stateVar id gameStateRef drawingArea healthLabels remainingLevels') delay >> return ()
    widgetShowAll window
    return False
