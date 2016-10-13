module Button.Msg exposing (..)

import Time exposing (Time)


type alias ButtonId =
    Int


type Msg
    = Activate ButtonId
    | DeActivate ButtonId
    | ShowAll Float Float
    | Show ButtonId Float Float Float
    | HideAll
    | Tick Time
