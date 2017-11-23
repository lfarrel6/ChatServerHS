# Chat Server
#### Liam Farrelly 14317869

# Running The Server

#### Run using stack 
[Get Stack!](https://docs.haskellstack.org/en/stable/README/)

Get a local GHC compiler: `stack setup`

Compile: `bash compile.sh`
Run the Server on the port number of your choice: `bash start.sh <port number>`

### Current test score: 64
### I do not feel that this reflects the standard of the Chat Server.

# Send Protocols
Action | Protocol
-------|---------
Helo (echo) | `HELO XXXX`
Stop Server | `KILL SERVICE`
Join Chatroom | `JOIN_CHATROOM: [chatroom name]`<br>`CLIENT_IP: 0`<br>`PORT: 0`<br>`CLIENT_NAME: [client identifying string]`
Leaving Chatroom | `LEAVE_CHATROOM: [room ref]`<br>`JOIN_ID: [id provided by server on join]`<br>`CLIENT_NAME: [client identifying string]`
Disconnect from Server | `DISCONNECT: 0`<br>`PORT: 0`<br>`CLIENT_NAME: [client identifying string]`
Messaging | `CHAT: [room ref]`<br>`JOIN_ID: [id provided to client on join]`<br>`CLIENT_NAME: [client identifying string]`<br>`MESSAGE: [message string terminated by '\n\n']`

# Server Response Protocols
Action | Protocol
-------|---------
Join Chatroom | `JOINED_CHATROOM: [chatroom name]`<br>`SERVER_IP: [ip address of chatroom]`<br>`PORT: [port of chatrooom]`<br>`ROOM_REF: [chatroom identifying integer]`<br>`JOIN_ID: [unique integer to identify user joining]`
Leaving Chatroom | `LEFT_CHATROOM: [room ref]`<br>`JOIN_ID: [id provided by server on join]`
Messaging | `CHAT: [room ref]`<br>`CLIENT_NAME: [client(sender) identifying string]`<br>`MESSAGE: [message string terminated by '\n\n']`
