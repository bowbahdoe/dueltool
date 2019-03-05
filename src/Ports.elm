port module Ports exposing (error, info, storeDuelState)

import Json.Encode exposing (Value)


port storeDuelState : Value -> Cmd msg


port error : String -> Cmd msg


port info : String -> Cmd msg
