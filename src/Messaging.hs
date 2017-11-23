module Messaging (Message(..)) where

-- <<Message
data Message = Notice String
             | Response String
             | Broadcast String
             | Command [[String]] String
             | Error String String
             deriving Show
-- >>
