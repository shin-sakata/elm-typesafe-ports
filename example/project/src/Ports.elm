port module Ports exposing (alertInt, alertVoid)


port alertInt : Int -> Cmd msg


port alertVoid : () -> Cmd msg
