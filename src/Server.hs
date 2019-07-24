{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever, forM_)
import Control.Exception (finally)
import Data.Maybe (isNothing, isJust)
import Data.IORef
import Data.Text (Text)
import qualified Data.Map as M
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status204)
import Network.Wai (Application, responseFile)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

-- Network-WebSockets
-- type ServerApp = PendingConnection -> IO ()
--

type ConnId = Int
type Client = (ConnId, WS.Connection)
type RoomPair = (Client, Maybe Client)

broadcast :: Text -> [Client] -> IO ()
broadcast msg = mapM_ $ (`WS.sendTextData` msg) . snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

addRoomPair :: Client -> [RoomPair] -> ([RoomPair], RoomPair)
addRoomPair c rps = ((c, Nothing):rps, (c, Nothing))

modRoomPair :: Client -> [RoomPair] -> [RoomPair] -> ([RoomPair], RoomPair)
modRoomPair c (rx : rxs) rps =
    let
        roomPair = filter (isJust . snd) rps ++ rxs
    in
    ((fst rx, Just c):roomPair, (fst rx, Just c))

removeRoomPair :: Int -> [RoomPair] -> ([RoomPair], [RoomPair])
removeRoomPair i rps =
    let
        filterDisconnectPair rp =
            case rp of
                (cl1, Just cl2) -> fst cl1 /= i && fst cl2 /= i
                (cl1, _) -> fst cl1 /= i
        filterConnectPair rp =
            case rp of
                (cl1, Just cl2) -> fst cl1 == i || fst cl2 == i
                (cl1, _) -> fst cl1 == i
    in
    (filter filterDisconnectPair rps, filter filterConnectPair rps)

chat :: IORef [Client] -> IORef [RoomPair] -> WS.ServerApp
chat ref pairRef pendingConn = do

    conn <- WS.acceptRequest pendingConn
    identifier <- atomicModifyIORef ref (addClient conn)

    -- pairing
    let client = (identifier, conn)
    pairRooms <- readIORef pairRef
    rp <- case filter (isNothing . snd) pairRooms of
               x : xs -> atomicModifyIORef pairRef (modRoomPair client (x:xs))

               _ -> atomicModifyIORef pairRef (addRoomPair client)

    case rp of
      (cl1, Just cl2) -> broadcast "pairling" [cl1, cl2]
      (cl1, Nothing) -> broadcast "wait" [cl1]

    flip finally (bothDisconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        conns <- readIORef ref
        pairRooms <- readIORef pairRef
        let roomPair = filter (filterRoomPair identifier) pairRooms

        case roomPair of
          ((cl1, Just cl2):_) -> broadcast msg [cl1, cl2]
          _ -> putStrLn "no connect"
    where
    -- def function in where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)
    disconnectPair identifier = atomicModifyIORef pairRef (removeRoomPair identifier)
    bothDisconnect identifier = do
                            disconnect identifier
                            roomPair <- disconnectPair identifier
                            case roomPair of
                              ((cl1, Just cl2):_) -> broadcast "the other one is disconnected!" [cl1, cl2]
                              _ -> putStrLn "room pair is deleted"

filterRoomPair :: ConnId -> RoomPair -> Bool
filterRoomPair cid rp =
    case rp of
        (cl1, Just cl2) -> fst cl1 == cid || fst cl2 == cid
        _ -> False

app :: Application
app req respond = respond $ responseFile status204 [] "" Nothing

start :: IO ()
start = do
    let port = 3000
    let setting = Warp.setPort port Warp.defaultSettings
    putStrLn $ "Your server is listening at http://localhost:" ++ show port ++ "/"
    ref <- newIORef []
    pairRef <- newIORef []
    Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref pairRef) app



