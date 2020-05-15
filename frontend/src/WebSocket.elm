port module WebSocket exposing (connect, listen, receive, sendMessage)


port listen : String -> Cmd msg


port receive : (String -> msg) -> Sub msg


port connect : (String -> msg) -> Sub msg


port sendMessage : String -> Cmd msg
