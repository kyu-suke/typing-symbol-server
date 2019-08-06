{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever, forM_)
import Control.Exception (finally)
import Data.Maybe (isNothing, isJust)
import Data.IORef
import Data.Text (Text, pack, unpack)
import qualified Data.Map as M
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status204)
import Network.Wai (Application, responseFile)
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.Random
import Data.List
import Data.String
import System.Environment
import qualified Data.Streaming.Network as DS


import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

-- Network-WebSockets
-- type ServerApp = PendingConnection -> IO ()
--

type ConnId = Int
type Client = (ConnId, WS.Connection)
-- type RoomPair = (Client, Maybe Client)
type RoomPair = (Client, Maybe Client, Config)

data Config = Config { message :: String
                     , targetChar :: String
                     , playerOneLeftChar :: String
                     , playerTwoLeftChar :: String
                     , p1Id :: Int
                     , p2Id :: Int
                     } deriving Show

charSet =
    ["0","1","2","3","4","5","6","7","8","9","!","#","$","%","&","'","(",")","*","+",",","-",".","/",":",";","<","=",">","?","@","[","]","^","_","`","{","|","}","~","\\\"","¥","\\\\"]

--charLength = 20
charLength = 2

setChars :: Int -> [String] -> [String] -> IO String
setChars i strs res = do
    r <- newStdGen
    let (idx, stdGen) = randomR (0, length strs - 1) r :: (Int, StdGen)
    let str = strs !! idx
    let resChars = res ++ [str]
    if length resChars == i then
      return $ intercalate "" resChars
    else
      setChars i strs resChars

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

trd' :: (a, b, c) -> c
trd' (_, _, c) = c

broadcast :: Text -> [Client] -> IO ()
broadcast msg = mapM_ $ (`WS.sendTextData` msg) . snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

addRoomPair :: Client -> Config -> [RoomPair] -> ([RoomPair], RoomPair)
addRoomPair cli con rps = ((cli, Nothing, con):rps, (cli, Nothing, con))

modRoomPair :: Client -> [RoomPair] -> [RoomPair] -> ([RoomPair], RoomPair)
modRoomPair c (rx : rxs) rps =
    let
        roomPair = filter (isJust . snd') rps ++ rxs
        newCfg = (trd' rx) {message = "pairing"}
    in
    ((fst' rx, Just c, newCfg):roomPair, (fst' rx, Just c, newCfg))

modRoomPairConfig :: Int -> Text -> String -> [RoomPair] -> ([RoomPair], RoomPair)
modRoomPairConfig i msg randomChars rps =
    let
        (cl1, cl2, cfg) = head $ filter (filterRoomPair i) rps
        tmpCfg = if message cfg == "pairing" then cfg {message = "typed"} else cfg

        replacedCfg = if fst cl1 == i then
                     tmpCfg {playerOneLeftChar = unpack msg}
                 else
                     tmpCfg {playerTwoLeftChar = unpack msg}

        newCfg
         |  playerOneLeftChar replacedCfg == "" && playerTwoLeftChar replacedCfg == ""  && message replacedCfg == "typed"  = replacedCfg {targetChar = randomChars, playerOneLeftChar = randomChars, playerTwoLeftChar = randomChars , message = "battle"}
         | (playerOneLeftChar replacedCfg == "" || playerTwoLeftChar replacedCfg == "") && message replacedCfg == "battle" = replacedCfg {message = "end"}
         | otherwise                                                                                                       = replacedCfg

        roomPair = filter (not . filterRoomPair i) rps
    in
    ((cl1, cl2, newCfg):roomPair, (cl1, cl2, newCfg))

removeRoomPair :: Int -> [RoomPair] -> ([RoomPair], [RoomPair])
removeRoomPair i rps =
    let
        filterDisconnectPair rp =
            case rp of
                (cl1, Just cl2, cfg) -> fst cl1 /= i && fst cl2 /= i
                (cl1, _, cfg) -> fst cl1 /= i
        filterConnectPair rp =
            case rp of
                (cl1, Just cl2, cfg) -> fst cl1 == i || fst cl2 == i
                (cl1, _, cfg) -> fst cl1 == i
    in
    (filter filterDisconnectPair rps, filter filterConnectPair rps)

chat :: IORef [Client] -> IORef [RoomPair] -> WS.ServerApp
chat ref pairRef pendingConn = do

    conn <- WS.acceptRequest pendingConn
    identifier <- atomicModifyIORef ref (addClient conn)

    randomChars <- setChars charLength charSet []
    rand <- newStdGen
    let (p1, gen) = random rand :: (Int, StdGen)
    let (p2,_) = random gen :: (Int, StdGen)
    rand <- newStdGen
    putStrLn "------------------------RANDOM------------------------"
    putStrLn randomChars
    putStrLn "------------------------------------------------------"
    -- pairing
    let client = (identifier, conn)
    pairRooms <- readIORef pairRef
    rp <- case filter (isNothing . snd') pairRooms of
               x : xs -> atomicModifyIORef pairRef (modRoomPair client (x:xs))

               _ -> atomicModifyIORef pairRef (addRoomPair client (Config "wait" "Ready?" "Ready?" "Ready?" p1 p2))
               --_ -> atomicModifyIORef pairRef (addRoomPair client (Config "ready" randomChars randomChars randomChars))

    case rp of
      (cl1, Just cl2, cfg) -> broadcast (makeRes cfg) [cl1, cl2]
      (cl1, Nothing, cfg) -> broadcast (makeRes cfg) [cl1]

    flip finally (bothDisconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        conns <- readIORef ref
        (cl1, Just cl2, cfg) <- atomicModifyIORef pairRef (modRoomPairConfig identifier msg randomChars)
        --broadcast (makeRes "typed" (makeConfig cfg identifier (unpack msg) (cl1, cl2))) [cl1, cl2]
        putStrLn $ show $ makeRes cfg
        broadcast (makeRes cfg) [cl1, cl2]

        -- pairRooms <- readIORef pairRef
        -- let roomPair = filter (filterRoomPair identifier) pairRooms
        -- case roomPair of
        --   ((cl1, Just cl2, cfg):_) -> broadcast (makeRes "typed" (makeConfig cfg identifier (unpack msg) (cl1, cl2))) [cl1, cl2]
        --   _ -> putStrLn "no connect"
    where
    -- def function in where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)
    disconnectPair identifier = atomicModifyIORef pairRef (removeRoomPair identifier)
    bothDisconnect identifier = do
                            disconnect identifier
                            roomPair <- disconnectPair identifier
                            case roomPair of
                              ((cl1, Just cl2, cfg):_) -> broadcast "the other one is disconnected!" [cl1, cl2]
                              _ -> putStrLn "room pair is deleted"

filterRoomPair :: ConnId -> RoomPair -> Bool
filterRoomPair cid rp =
    case rp of
        (cl1, Just cl2, cfg) -> fst cl1 == cid || fst cl2 == cid
        _ -> False

makeRes :: Config -> Text
makeRes cfg =
  pack $ "{" ++
  " \"message\": \"" ++ message cfg ++ "\"," ++
  " \"targetChar\": \"" ++ targetChar cfg ++ "\"," ++
  " \"playerOneLeftChar\": \"" ++ playerOneLeftChar cfg ++ "\"," ++
  " \"playerTwoLeftChar\": \"" ++ playerTwoLeftChar cfg ++ "\"," ++
  " \"p1Id\": \"" ++ show(p1Id cfg) ++ "\"," ++
  " \"p2Id\": \"" ++ show(p2Id cfg) ++ "\"" ++
  "}"

makeConfig :: Config -> Int -> String -> (Client, Client) -> Config
makeConfig cfg i s (cl1, cl2) =
    if i == fst cl1 then
        cfg {playerOneLeftChar = s}
    else
        cfg {playerTwoLeftChar = s}

app :: Application
app req respond = respond $ responseFile status204 [] "" Nothing

start :: IO ()
start = do

    maybeHost <- lookupEnv "SERVER_HOST"
    let host = case maybeHost of
                 Just h -> h
                 _  -> "localhost"

    maybePort <- lookupEnv "SERVER_PORT"
    let port = case maybePort of
                 Just p -> p
                 _ -> "3000"

    let setting = Warp.setHost (fromString host) $ Warp.setPort (read port) Warp.defaultSettings
    putStrLn $ "Your server is listening at " ++ host ++ ":" ++ port ++ "/"
    ref <- newIORef []
    pairRef <- newIORef []
    Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref pairRef) app



