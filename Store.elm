port module Store exposing (load, save)

import Json.Decode as Decode
import Json.Encode as Encode


port load : (Decode.Value -> msg) -> Sub msg


port save : Encode.Value -> Cmd msg
