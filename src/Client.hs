{-# LANGUAGE RecordWildCards #-}

module Client (Client(..), newClient, sendMessage) where

import Messaging
import Control.Concurrent.STM
import Control.Concurrent
import Network
import System.IO

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

-- <<SendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan
-- >>
