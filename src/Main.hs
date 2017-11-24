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
import Network.Socket (close)
import Control.Monad
import Text.Printf
import System.Environment

-- ________________
-- Data Structures 
-- ________________

import Chatroom
import Client
import Messaging

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

-- >>

-- <<ServSimple

--simple helo/kill handler

simpleServer :: Handle -> Int -> IO ()
simpleServer hdl port = listen
 where
  listen = do 
   msg <- hGetLine hdl
   case words msg of
    ["HELO",text] -> do
      reply $ "HELO " ++ msg ++ "\n134.226.44.141\nPort:" ++ show port ++ "\nStudentID:14317869\n"
      simpleServer hdl port
    ["KILL_SERVICE"] -> void $ putStrLn "rip server"
    _ -> putStrLn "Unknown command" >> simpleServer hdl port
    where
     reply = hPutStrLn hdl

-- >>

-- <<UserHandler

userHandler :: Server -> Socket -> Int -> IO ()
userHandler serv sock port = do
  (handle, host, clientPort) <- accept sock
  printf "Accepted connection from %s: %s\n" host (show clientPort)
  forkFinally (talk handle serv port) (\_ -> hClose handle)
  userHandler serv sock port

-- >>

-- <<NotifyRoom
notifyRoom :: Server -> Int -> Message -> IO Bool
notifyRoom server roomRef msg = do
  roomsList <- atomically $ readTVar server
  let maybeRoom = Map.lookup roomRef roomsList
  case maybeRoom of
   Nothing    -> debug ("room does not exist " ++ show roomRef) >> return True
   Just aRoom -> sendRoomMessage msg aRoom >> return True

-- >>

-- <<talk 

-- handler for new connections

talk :: Handle -> Server -> Int -> IO ()
talk handle server port = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  debug ">Server Ready..."
  readOp
  where
   readOp = do
     op <- hGetLine handle
     case words op of
       
       ["JOIN_CHATROOM:",roomName] -> do
         arguments <- getArgs (joinArgs-1)
         
         case map words arguments of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
             client <- newClient name (hash name) handle
             joinChatroom client server port roomName
             let msgLines = "CHAT:"++(show $ hash roomName)++"\nCLIENT_NAME:"++name++"\nMESSAGE:"++name ++ " has joined this chatroom.\n"
             notifyRoom server (hash roomName) $ Broadcast msgLines
             void $ runClient server port client

           _ -> output "Unrecognized command" >> readOp
              
       _ -> output "Unreconized command" >> debug op >> readOp
       where
        output = hPutStrLn handle 
        getArgs n = replicateM n $ hGetLine handle

-- >>

-- Main server logic

runClient :: Server -> Int -> Client -> IO ()
runClient serv port client@Client{..} = do
  race server receive
  return ()
  where
   receive = forever $ do
     msg <- hGetLine clientHandle
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
       _                           -> debug msg >> throwError
       where
        send :: [String] -> String -> IO ()
        send args initialArg = atomically   $ sendMessage client $ Command (map words args) initialArg
        throwError           = atomically   $ sendMessage client $ Error "Error 1" "Unrecognised Command"
        getArgs n            = replicateM n $ hGetLine clientHandle
   server = join $ atomically $ do
     msg <- readTChan clientSendChan
     return $ do 
       continue <- handleMessage serv port client msg
       when continue server

-- >>

-- <<handleMessage

-- Intrepret incoming Messages

handleMessage :: Server -> Int -> Client -> Message -> IO Bool
handleMessage server port client@Client{..} message =
  case message of
    Notice    msg       -> output $ msg
    Response  msg       -> output $ msg
    Broadcast msg       -> output $ msg
    Error heading body  -> output $ "->" ++ heading ++ "<-\n" ++ body
    Command msg mainArg -> case msg of

      --join chatroom

      [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
        let msgLines = "CHAT:"++(show $ hash mainArg)++"\nCLIENT_NAME:"++clientName++"\nMESSAGE:"++clientName ++ " has joined this chatroom.\n"
        joinChatroom client server port mainArg >> notifyRoom server (hash mainArg) (Broadcast msgLines)

      --leave chatroom

      [["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
        leave' client server (read mainArg :: Int) (read id :: Int)
        return True

      --disconnect

      [["PORT:",_],["CLIENT_NAME:",name]] -> debug "disconnecting user" >> removeClient server client >> return False

      --send message

      [["JOIN_ID:",id],["CLIENT_NAME:",name],("MESSAGE:":msgToSend),[]] ->
        notifyRoom server (read mainArg :: Int) $ Broadcast ("CHAT: " ++ mainArg ++ "\nCLIENT_NAME: " ++ name ++ "\nMESSAGE: "++ unwords msgToSend ++"\n")
      
      --wildcard
      
      _ -> do
        atomically   $ sendMessage client $ Error "Error 1" "Unrecognised Args"
        mapM_ (debug . unwords) msg
        return True
      where
       reply replyMsg = atomically $ sendMessage client replyMsg
  where
   output s = do debug (clientName ++ " receiving\\/\n" ++ s) >> hPutStrLn clientHandle s; return True

-- >>

-- <<main

-- Simple main to take connections and pass them to talk

main :: IO ()
main = withSocketsDo $ do 
 args <- getArgs
 let port = read (head args) :: Int
 server <- newServer
 sock <- listenOn $ portNum port
 printf "Listening on port %d\n" port
 (handle, host, clientPort) <- accept sock
 printf "Accepted connection from %s: %s\n" host (show clientPort)
 forkFinally (simpleServer handle port) (\_ -> void (sClose sock))
 userHandler server sock port
 return ()
 where
  portNum n = PortNumber $ fromIntegral n

-- >>

-- <<debug

-- Simple debug function to print debug information, used to remove random putStrLns throughout code

debug :: String -> IO ()
debug = putStrLn

-- >>
