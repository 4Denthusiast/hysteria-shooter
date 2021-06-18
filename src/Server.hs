module Server (
    runServer
) where

import Control.Concurrent.MVar
import Control.Monad
import Network.Simple.TCP

import Multiplayer

data ServerState = Connecting [(Socket, [Float])] | InGame [Socket] | Disconnecting [Socket]

runServer :: IO ()
runServer = newMVar (Connecting []) >>= \stateVar -> serve HostAny "54503" (serveClient stateVar)

serveClient :: MVar ServerState -> (Socket, SockAddr) -> IO ()
serveClient stateVar (sock, remoteAddr) = do
    state <- takeMVar stateVar
    connectionRequest <- recvNetMessage sock
    case (state, connectionRequest) of
        (InGame _, _) -> sendNetMessage sock ServerIsBusy >> putStrLn "Refused connection. Already busy." >> putMVar stateVar state
        (Disconnecting _, _) -> sendNetMessage sock ServerIsBusy >> putStrLn "Refused connection. Already shutting down game." >> putMVar stateVar state
        (Connecting sockets, Just (PlayerConnected color)) -> do
            putStrLn "Accepted connection."
            sendNetMessage sock (YourIdIs $ length sockets)
            forM sockets $ \(otherSock, otherColor) -> do
                sendNetMessage otherSock (PlayerConnected color)
                sendNetMessage sock (PlayerConnected otherColor)
            sendNetMessage sock (PlayerConnected color)
            putMVar stateVar (Connecting $ sockets ++ [(sock, color)])
            serveClientLoop stateVar sock
        _ -> putMVar stateVar state

serveClientLoop :: MVar ServerState -> Socket -> IO ()
serveClientLoop stateVar sock = do
    messageM <- recvNetMessage sock
    state <- takeMVar stateVar
    let otherSockets = case state of
            Connecting others -> map fst others
            InGame others -> others
            Disconnecting others -> others
    case messageM of
        Nothing -> disconnectClient stateVar state otherSockets sock
        Just Disconnect -> disconnectClient stateVar state otherSockets sock
        Just message -> do
            putStrLn "Starting game."
            forM otherSockets (\otherSock -> sendNetMessage otherSock message)
            putMVar stateVar (InGame otherSockets)
            serveClientLoop stateVar sock

disconnectClient :: MVar ServerState -> ServerState -> [Socket] -> Socket -> IO ()
disconnectClient stateVar state socks sock = do
    closeSock sock
    let socks' = filter (/= sock) socks
    case state of
        Disconnecting _ -> return ()
        _ -> putStrLn "Starting to disconnect." >> forM_ socks' (\sock' -> sendNetMessage sock' Disconnect)
    putStrLn "Disconnecting client."
    putMVar stateVar $ case socks' of
        [] -> Connecting []
        _ -> Disconnecting socks'
    if null socks' then putStrLn "Completed disconnection. Awaiting new connections." else return ()
