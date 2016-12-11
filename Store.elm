port module Store exposing (..)

import Json.Encode as Encode


port save : Encode.Value -> Cmd msg
