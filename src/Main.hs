{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Concurrent
import GHC.Conc
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.Hashable
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

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

--kill service
killService :: String
killService = "KILL"

-- >>

-- <<talk 

-- handler for new connections

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  debug ">Server Ready..."
  readOp
  return ()
  where
   readOp = do
     op <- hGetLine handle
     debug $ op ++ " received pre client creation"
     case words op of
       ["HELO","BASE_TEST"] -> do
         echo $ "HELO text\nIP:134.226.44.141\nPort:" ++ (show port) ++ "\nStudentID:14317869\n"
         readOp
       ["KILL_SERVICE"] -> output "RIP" >> return ()
       ["JOIN_CHATROOM:",roomName] -> do
         arguments <- getArgs (joinArgs-1)
         --output roomName
         case map words arguments of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do

             client <- newClient name (hash name) handle
             joinChatroom client server roomName
             debug $ "***Welcome, "++name++"***"
             debug $ name++" entered " ++ roomName ++ " //// " ++ show (hash roomName)
             let msgLines = "CHAT:"++(show $ (hash roomName))++"\nCLIENT_NAME:"++name++"\nMESSAGE:"++name ++ " has joined this chatroom.\n"
             notifyRoom (hash roomName) $ Broadcast msgLines
             runClient server client >> endClient client --(removeClient server client >> return ())
           _ -> output "Unrecognized command" >> readOp
           where
            notifyRoom roomRef msg = do
              roomsList <- atomically $ readTVar server
              let maybeRoom = Map.lookup roomRef roomsList
              case maybeRoom of
               Nothing    -> debug ("room does not exist " ++ (show roomRef)) >> return True
               Just aRoom -> sendRoomMessage msg aRoom >> return True
            endClient client = do
              debug "Client deleted entirely"
              return ()
              --removeClient server client
       _ -> output "Unreconized command" >> debug op >> readOp
       where
        output = hPutStrLn handle 
        getArgs n = replicateM n $ hGetLine handle
        echo s = do
                  debug $ s ++ " being echoed"
                  output s
                  input <- hGetLine handle
                  echo input

-- >>

-- <<runClient

-- Main server logic

runClient :: Server -> Client -> IO ()
runClient serv client@Client{..} = do
  debug "hello"
  race server receive
  debug "race finished"
  return ()
  where
   receive = forever $ do
     debug "receiving"
     msg <- hGetLine clientHandle
     debug $ msg ++ " received"
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
         putStrLn "disconnect command"
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

-- Intrepret incoming Messages

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
        debug ("joining joinRef = " ++ show (clientID + (hash mainArg)))
        let msgLines = "CHAT:"++(show $ (hash mainArg))++"\nCLIENT_NAME:"++clientName++"\nMESSAGE:"++clientName ++ " has joined this chatroom.\n"
        joinChatroom client server mainArg >> notifyRoom (hash mainArg) (Broadcast msgLines)

      --leave chatroom

      [["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
        debug ("leave room joinref = " ++ id)
        leave' client server (read mainArg :: Int) (read id :: Int)
        debug "chatroom left success"
        return True

      --disconnect

      [["PORT:",_],["CLIENT_NAME:",name]] -> debug "disconnecting user" >> removeClient server client >> return False

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
        mapM_ debug $ map unwords msg
        debug "Error didnt recognise command"
        return True
      where
       reply replyMsg = atomically $ sendMessage client replyMsg
       notifyRoom roomRef msg = do
         roomsList <- atomically $ readTVar server
         let maybeRoom = Map.lookup roomRef roomsList
         case maybeRoom of
           Nothing    -> debug ("room does not exist " ++ (show roomRef)) >> return True
           Just aRoom -> sendRoomMessage msg aRoom >> return True
  where
   output s = do debug (clientName ++ " receiving\\/\n" ++ s) >> hPutStrLn clientHandle s; return True

-- >>

-- <<main

-- Simple main to take connections and pass them to talk

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

-- Simple debug function to print debug information, used to remove random putStrLns throughout code

debug :: String -> IO ()
debug = putStrLn

-- >>
