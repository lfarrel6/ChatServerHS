{-# LANGUAGE RecordWildCards #-}

module Chatroom (Server, Chatroom, removeClient, newServer,
                newChatroom, getChatroom, joinChatroom,
                leaveChatroom, leave', deleteChatroom, sendRoomMessage) where

import Messaging
import qualified Client as C
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Hashable
import Network
import qualified Data.List as List

-- <<Server

-- type definition for Server, constructor function for new empty Servers

type Server = TVar (Map Int Chatroom)

newServer :: IO Server
newServer = newTVarIO Map.empty

-- >>

-- <<Chatroom

-- Data type for Chatroom

data Chatroom = Chatroom
  { roomName :: String
  , roomRef  :: Int
  , members  :: TVar (Map Int C.Client)
  }

-- >>

-- <<newChatroom

-- Constructor function for new Chatroom with first client in it

newChatroom :: C.Client -> String -> STM Chatroom
newChatroom joiner@C.Client{..} room = do
  clientList <- newTVar $ Map.insert clientID joiner Map.empty
  return Chatroom { roomName = room
                  , roomRef  = hash room
                  , members  = clientList
                  }

-- >>

-- <<getChatroom

-- Search for chatroom in server listings, returns a maybe chatroom

getChatroom :: Int -> Server -> STM (Maybe Chatroom)
getChatroom roomRef serv = do
  rooms <- readTVar serv
  case Map.lookup roomRef rooms of
   Nothing -> return Nothing
   Just x  -> return $ Just x

-- >>

-- <<joinChatroom

-- Insert new client into the chatroom, if the chatroom doesnt exist, create a new one and insert user into that

joinChatroom :: C.Client -> Server -> Int -> String -> IO ()
joinChatroom joiner@C.Client{..} rooms port name = atomically $ do
  roomList <- readTVar rooms
  case Map.lookup (hash name) roomList of
  --if the room doesn't exist we want to create the room
    Nothing    -> do
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
     sendResponse ref name = C.sendMessage joiner (Response $ "JOINED_CHATROOM:"++name++"\nSERVER_IP:0.0.0.0\nPORT:"++show (fromIntegral port) ++ "\nROOM_REF:" ++ show ref ++"\nJOIN_ID:" ++ show (ref+clientID))

-- >>

-- <<removeClient

-- Remove client from entire server

removeClient :: Server -> C.Client -> IO ()
removeClient serv toRemove@C.Client{..} = do
  rooms <- atomically $ readTVar serv
  let roomNames = reverse $ Prelude.map (\room -> roomName room) (Map.elems rooms)
  debug "roomNames obtained"

  debug $ show roomNames
  mapM_ (\room -> kickFrom room) roomNames
  debug "user dced"
  where
   kickFrom room = do 
     debug ("removing " ++ clientName ++ " from " ++ room)
     disconnectClient toRemove serv (hash room)

-- >>

-- <<disconnectClient

-- special case of leaving, requires different messages and checks membership before deleting

disconnectClient :: C.Client -> Server -> Int -> IO ()
disconnectClient c@C.Client{..} server roomRef = do
   rooms <- atomically $ readTVar server
   case Map.lookup roomRef rooms of
    Nothing    -> return ()
    Just aRoom -> do
     memberListing <- atomically $ readTVar $ members aRoom
     case Map.lookup clientID memberListing of
      Nothing   -> return ()
      Just user -> do
       atomically $ notify $ Map.elems memberListing
       let newList = Map.delete clientID memberListing
       atomically $ writeTVar (members aRoom) newList
     where
      notify l     = mapM_ (\x -> C.sendMessage x notification) l
      notification = (Broadcast $ "CHAT:" ++ (show roomRef) ++ "\nCLIENT_NAME:" ++ clientName ++ "\nMESSAGE:" ++ clientName ++ " has left this chatroom.\n")


-- >>

-- <<leaveChatroom

-- Remove user from given Chatroom, used when joinID was not provided, acts as a wrapper for leave'

leaveChatroom :: C.Client -> Server -> Int -> IO ()
leaveChatroom client@C.Client{..} server roomRef = leave' client server roomRef (roomRef+clientID)

-- >>

-- <<leave'

-- Remove users from Chatroom given the joinID

leave' :: C.Client -> Server -> Int -> Int -> IO ()
leave' client@C.Client{..} server roomRef joinRef = do
  roomList <- atomically $ readTVar server
  case Map.lookup roomRef roomList of
    Nothing    -> debug "Room does not exist"
    Just aRoom -> do
      atomically $ C.sendMessage client (Response $ "LEFT_CHATROOM:" ++ show roomRef ++ "\nJOIN_ID:" ++ show joinRef)
      removeUser
      where
       removeUser = atomically $ do
         clientList <- readTVar (members aRoom)
         let roomMembers = Map.elems clientList
         mapM_ (\aClient -> C.sendMessage aClient notification) roomMembers
         let newList = Map.delete (hash clientName) clientList
         writeTVar (members aRoom) newList
       notification = (Broadcast $ "CHAT:" ++ (show roomRef) ++ "\nCLIENT_NAME:" ++ clientName ++ "\nMESSAGE:" ++ clientName ++ " has left this chatroom.\n")

-- >>

-- <<deleteChatroom

-- Delete Chatroom if it doesn't exist

deleteChatroom :: Server -> Int -> IO ()
deleteChatroom serv ref = atomically $ do 
  list <- readTVar serv
  case Map.lookup ref list of
    Nothing    -> return ()
    Just aRoom -> do
      let newList = Map.delete ref list
      writeTVar serv newList

-- >>

-- <<SendRoomMessage

-- Send message to every member of Chatroom

sendRoomMessage :: Message -> Chatroom -> IO ()
sendRoomMessage msg room@Chatroom{..} = do
  atomically $ notifyRoom
  where
   notifyRoom = do
    memberList <- readTVar members
    let roomMembers = Map.elems memberList
    mapM_ (\aClient -> C.sendMessage aClient msg) roomMembers

-- >>

-- <<debug

-- for information see debug in main

debug :: String -> IO()
debug = putStrLn

-- >>

