port module Ports exposing (saveAuthToken)


port saveAuthToken : Maybe String -> Cmd msg
