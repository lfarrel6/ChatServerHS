{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Concurrent
import GHC.Conc
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Hashable
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

-- __________________________________
-- Data Structures and initialisation
-- __________________________________

-- <<Server
type Server = TVar (Map Int Chatroom)

newServer :: IO Server
newServer = newTVarIO Map.empty
-- >>

-- <<Chatroom
data Chatroom = Chatroom
  { roomName :: String
  , roomRef  :: Int
  , members  :: TVar (Map Int Client)
  }

newChatroom :: Client -> String -> STM Chatroom
newChatroom joiner@Client{..} room = do
  clientList <- newTVar $ Map.insert clientID joiner Map.empty
  return Chatroom { roomName = room
                  , roomRef  = hash room
                  , members  = clientList
                  }

joinChatroom :: Client -> Server -> String -> IO ()
joinChatroom joiner@Client{..} rooms name = atomically $ do
  roomList <- readTVar rooms
  case Map.lookup (hash name) roomList of
  --if the room doesn't exist we want to create the room
    Nothing -> do
      room <- newChatroom joiner name

      let updatedRoomList = Map.insert (roomRef room) room roomList
      writeTVar rooms updatedRoomList
      sendResponse (roomRef room) (roomName room)
    Just aRoom -> do
      clientList <- readTVar (members aRoom)
      let newClientList = Map.insert clientID joiner clientList
      writeTVar (members aRoom) newClientList
      sendResponse (roomRef aRoom) (roomName aRoom)
    where
     sendResponse ref name = sendMessage joiner (Response $ "JOINED_CHATROOM:"++name++"\nSERVER_IP:0.0.0.0\nPORT:"++show (fromIntegral port) ++ "\nROOM_REF:" ++ show ref ++"\nJOIN_ID:" ++ show (ref+clientID))

leaveChatroom :: Client -> Server -> Int -> IO ()
leaveChatroom client@Client{..} server roomRef = do
  roomList <- atomically $ readTVar server
  case Map.lookup roomRef roomList of
    Nothing    -> putStrLn "Room does not exist" 
    Just aRoom -> do
      atomically $ sendMessage client (Response $ "LEFT_CHATROOM:" ++ show roomRef ++ "\nJOIN_ID:" ++ (show $ clientID + roomRef)++"\n")
      removeUser -- >> sendRoomMessage notification aRoom >> atomically (sendMessage client notification)
      putStrLn $ clientName++" left " ++ (roomName aRoom)
      putStrLn $ "removal notif looks like: " ++ (show notification)
      where
       removeUser = atomically $ do
         clientList <- readTVar (members aRoom)
         let roomMembers = Map.elems clientList
         mapM_ (\aClient -> sendMessage aClient notification) roomMembers
         let newList = Map.delete (hash clientName) clientList
         writeTVar (members aRoom) newList
       notification = (Broadcast $ "CHAT:" ++ (show roomRef) ++ "\nJOIN_ID:" ++ (show $ roomRef + clientID) ++ "\nCLIENT_NAME:" ++ clientName ++ "\nMESSAGE:" ++ clientName ++ " has left this chatroom.\n\n")

deleteChatroom :: Server -> Int -> IO ()
deleteChatroom serv ref = atomically $ do 
  list <- readTVar serv
  case Map.lookup ref list of
    Nothing    -> return ()
    Just aRoom -> do
      let newList = Map.delete ref list
      writeTVar serv newList
-- >>

-- <<Client
data Client = Client
   { clientName     :: String
   , clientID       :: Int
   , clientHandle   :: Handle
   , clientSendChan :: TChan Message
   }

newClient :: String -> Int -> Handle -> IO Client
newClient name id handle = do
  c <- newTChanIO
  return Client { clientName     = name
                , clientID       = id
                , clientHandle   = handle
                , clientSendChan = c
                }
-- >>

-- <<Message
data Message = Notice String
             | Response String
             | Broadcast String
             | Command [[String]] String
             | Error String String
             deriving Show
-- >>

-- <<SendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan
-- >>

-- <<SendRoomMessage
sendRoomMessage :: Message -> Chatroom -> IO ()
sendRoomMessage msg room@Chatroom{..} = do
  atomically $ notifyRoom
  putStrLn $ "sRM " ++ (show msg)
  where
   notifyRoom = do
    memberList <- readTVar members
    let roomMembers = Map.elems memberList
    mapM_ (\aClient -> sendMessage aClient msg) roomMembers
-- >>

-- _____________________
-- Readability functions
-- _____________________

-- <<Port
port :: Int
port = 44444
-- >>

-- <<Protocol Arguments

--join
joinArgs :: Int
joinArgs = 4
--leave
leaveArgs :: Int
leaveArgs = 3
--disconnect
disconnectArgs :: Int
disconnectArgs = 3
--send
sendMsgArgs :: Int
sendMsgArgs = 5
--kill service
killService :: String
killService = "KILL"

-- >>

-- <<talk - handler for new connections
talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  putStrLn ">Server Ready..."
  readOp
  return ()
  where
   readOp = do
     op <- hGetLine handle
     case words op of
       ["HELO",_] -> do
         output $ "HELO text\nIP:134.226.44.141\nPort:" ++ (show port) ++ "\nStudentID:14317869\n"
         readOp
       ["KILL_SERVICE"] -> output "RIP" >> return ()
       ["JOIN_CHATROOM:",roomName] -> do
         arguments <- getArgs (joinArgs-1)
         --output roomName
         case map words arguments of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do

             client <- newClient name (hash name) handle
             joinChatroom client server roomName
             putStrLn $ "***Welcome, "++name++"***"
             putStrLn $ name++" entered " ++ roomName ++ " //// " ++ show (hash roomName)
             let msgLines = "CHAT:"++(show $ (hash roomName))++"\nCLIENT_NAME:"++name++"\nMESSAGE:"++name ++ " has joined this chatroom.\n"
             notifyRoom (hash roomName) $ Broadcast msgLines
             runClient server client >> endClient client --(removeClient server client >> return ())
           _ -> output "Unrecognized command" >> readOp
           where
            notifyRoom roomRef msg = do
              roomsList <- atomically $ readTVar server
              let maybeRoom = Map.lookup roomRef roomsList
              case maybeRoom of
               Nothing    -> putStrLn ("room does not exist " ++ (show roomRef)) >> return True
               Just aRoom -> sendRoomMessage msg aRoom >> return True
            endClient client = do
              putStrLn "Client is being deleted entirely"
              removeClient server client >> return ()
       _ -> output "Unreconized command" >> debug op >> readOp
       where
        output = hPutStrLn handle 
        getArgs n = replicateM n $ hGetLine handle

-- >>

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv client@Client{..} = do
  putStrLn "hello"
  race server receive
  putStrLn "race finished"
  return ()
  where
   receive = forever $ do
     putStrLn "receiving"
     msg <- hGetLine clientHandle
     putStrLn $ msg ++ " received"
     case words msg of
       ["JOIN_CHATROOM:",roomName] -> do
         cmdLineArgs <- getArgs (joinArgs-1)
         send cmdLineArgs roomName
       ["LEAVE_CHATROOM:",roomRef] -> do
         cmdLineArgs <- getArgs (leaveArgs-1)
         mapM_ putStrLn cmdLineArgs
         send cmdLineArgs roomRef
       ["DISCONNECT:",ip]          -> do
         cmdLineArgs <- getArgs (disconnectArgs-1)
         send cmdLineArgs ip
       ["CHAT:",roomRef]           -> do
         cmdLineArgs <- getArgs (sendMsgArgs-1)
         send cmdLineArgs roomRef
       ["KILL_SERVICE"]            -> do
         send [killService] killService
       _                           -> debug msg >> throwError
       where
        send :: [String] -> String -> IO ()
        send args initialArg = atomically   $ sendMessage client $ Command (map words args) initialArg
        throwError           = atomically   $ sendMessage client $ Error "Error 1" "Unrecognised Command"
        getArgs n            = replicateM n $ hGetLine clientHandle
   server = join $ atomically $ do
     msg <- readTChan clientSendChan
     return $ do 
       continue <- handleMessage serv client msg
       when continue $ server
-- >>

-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
    Notice    msg       -> output $ msg
    Response  msg       -> output $ msg
    Broadcast msg       -> output $ msg
    Error heading body  -> output $ "->" ++ heading ++ "<-\n" ++ body
    Command msg mainArg -> case msg of

      --join chatroom

      [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
        joinChatroom client server mainArg
        putStrLn ("joined " ++ mainArg)
        let msgLines = "CHAT:"++(show $ (hash mainArg))++"\nCLIENT_NAME:"++clientName++"\nMESSAGE:"++clientName ++ " has joined this chatroom.\n"
        notifyRoom (hash mainArg) $ Broadcast msgLines

      --leave chatroom

      [["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
        leaveChatroom client server (read mainArg :: Int)
        putStrLn "chatroom left success"
        return True

      --disconnect

      [["PORT:",_],["CLIENT_NAME:",name]] -> return False

      --send message

      [["JOIN_ID:",id],["CLIENT_NAME:",name],("MESSAGE:":msgToSend),[]] -> do
        notifyRoom (read mainArg :: Int) $ Broadcast ("CHAT: " ++ mainArg ++ "\nCLIENT_NAME: " ++ name ++ "\nMESSAGE: "++(unwords msgToSend)++"\n")

      --kill service

      [["KILL"]]                         -> do
        if mainArg == killService then return False
        else return True
      
      --wildcard
      
      _ -> do
        atomically   $ sendMessage client $ Error "Error 1" "Unrecognised Args"
        mapM_ putStrLn $ map unwords msg
        putStrLn "Error didnt recognise command"
        return True
      where
       reply replyMsg = atomically $ sendMessage client replyMsg
       notifyRoom roomRef msg = do
         roomsList <- atomically $ readTVar server
         let maybeRoom = Map.lookup roomRef roomsList
         case maybeRoom of
           Nothing    -> putStrLn ("room does not exist " ++ (show roomRef)) >> return True
           Just aRoom -> sendRoomMessage msg aRoom >> return True
  where
   output s = do putStrLn ("user receiving\\/\n" ++ s) >> hPutStrLn clientHandle s; return True
-- >>

-- <<removeClient
-- want to remove client from entire server
removeClient :: Server -> Client -> IO ()
removeClient serv toRemove@Client{..} = do
  rooms <- atomically $ readTVar serv
  let roomNames = Prelude.map (\room -> roomName room) (Map.elems rooms)
  putStrLn $ show roomNames
  mapM_ (\room -> kick room) roomNames
  where
   kick room = do 
     leaveChatroom toRemove serv (hash room) >> putStrLn (clientName ++ " removed from " ++ room)
-- >>

-- <<main
main :: IO ()
main = withSocketsDo $ do 
 server <- newServer
 sock <- listenOn (PortNumber (fromIntegral port))
 printf "Listening on port %d\n" port
 forever $ do
   (handle, host, port) <- accept sock
   printf "Accepted connection from %s: %s\n" host (show port)
   forkFinally (talk handle server) (\_ -> hClose handle)
-- >>

-- <<debug
debug :: String -> IO ()
debug = putStrLn
-- >>
