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
import GHC.Generics
import Graphics.UI.Gtk hiding (Socket)
import Network.Simple.TCP
import System.Clock
import System.IO.Error

import Game
import GameControl

data NetMessage = PlayerConnected [Float] | YourIdIs Int | ServerIsBusy | Disconnect | StartLevel Int GameState | InputFrom Int Input deriving (Eq, Generic, Serial, Show)

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

updateStateWithMessage :: Int -> NetMessage -> MultiplayerState -> MultiplayerState
updateStateWithMessage id (InputFrom id' act) state = if id == id' then state else state{multiplayerInputs = addInput act id' $ multiplayerInputs state}
updateStateWithMessage id (StartLevel tick level) state = state{multiplayerPendingLevel = Just (tick, level)}
updateStateWithMessage _ _ _ = error "Unexpected net message."

guessInputs :: [Input] -> [Input]
guessInputs (x0:x1:xs) = x0 : guessInputs (x1:xs)
guessInputs [x] = repeat x
guessInputs [] = error "Empty input queue."

multiplayerTick :: Window -> IORef InputState -> Socket -> MVar MultiplayerState -> Int -> IORef GameState -> DrawingArea -> [Label] -> IO Bool
multiplayerTick window inputRef sock stateVar id gameStateRef drawingArea healthLabels = do
    (input, inputState) <- decodeInput <$> readIORef inputRef
    writeIORef inputRef inputState
    sendNetMessage sock $ InputFrom id input
    MultiplayerState startTime referenceState referenceTick currentTick pendingLevel inputs <- takeMVar stateVar
    let inputs' = addInput input id inputs
    let definiteInputs = takeWhile ((== length inputs) . length) $ tail $ transpose inputs'
    let referenceState' = foldl (flip stepGame) referenceState definiteInputs
    let referenceTick' = referenceTick + length definiteInputs
    let currentTick' = currentTick + 1
    let guessedInputs = take (currentTick + 1 - referenceTick') $ drop (length definiteInputs + 1) $ transpose (map guessInputs inputs')
    let guessedGameState = foldl (flip stepGame) referenceState' guessedInputs
    writeIORef gameStateRef guessedGameState
    let (GameState _ _ _ predictedPlayerStates) = guessedGameState
    forM (zip3 healthLabels predictedPlayerStates [0..]) $ \(label, PlayerState _ _ _ _ health _, id') -> labelSetText label ("P"++show id'++":\n"++ if health <= 0 then "Dead" else show health ++ "HP")
    let inputs'' = map (drop (length definiteInputs)) inputs'
    putMVar stateVar $ MultiplayerState startTime referenceState' referenceTick' currentTick' pendingLevel inputs''
    redrawCanvas drawingArea gameStateRef
    widgetShowAll window
    time <- getTime Monotonic
    let nextTickTime = fromNanoSecs (fromIntegral $ currentTick' * 150000000) + startTime
    let delay = max 1 $ fromInteger $ div (toNanoSecs (nextTickTime - time)) 1000000
    timeoutAdd (multiplayerTick window inputRef sock stateVar id gameStateRef drawingArea healthLabels) delay
    return False