port module PersistentState exposing (loadState, onLoadState, saveState)


port loadState : () -> Cmd msg


port onLoadState : (Maybe String -> msg) -> Sub msg


port saveState : String -> Cmd msg
