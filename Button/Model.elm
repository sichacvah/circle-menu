module Button.Model exposing (..)

import Time exposing (Time, millisecond)


type State
    = Growing
    | Shrinking
    | Here


type alias Button =
    { hint : String
    , iconSrc : String
    , activeIconSrc : String
    , active : Bool
    , angle : Float
    , innerRadius : Float
    , outerRadius : Float
    , x : Float
    , y : Float
    , id : Int
    , clock : Time
    , state : State
    }



-- TODO SET FROM APP MODEL


size : Float
size =
    500


initBtn : Float -> Float -> Float -> Float -> Float -> Int -> ( Int, String, String ) -> Button
initBtn angle x y outerRadius innerRadius index ( id, hint, iconSrc ) =
    let
        imgDir =
            "./Button/img/"

        ext =
            ".svg"

        active =
            "active/"
    in
        { hint = hint
        , iconSrc = imgDir ++ iconSrc ++ ext
        , activeIconSrc = imgDir ++ active ++ iconSrc ++ ext
        , active = False
        , id = id
        , angle = angle
        , innerRadius = innerRadius
        , outerRadius = outerRadius
        , x = x
        , y = y
        , clock = 0
        , state = Here
        }


initialButtons : Float -> List Button
initialButtons size =
    let
        x =
            size / 2

        y =
            size / 2

        outerRadius =
            size * 0.25

        innerRadius =
            outerRadius * 0.35

        buttons =
            [ ( 1, "accessibility", "accessibility" )
            , ( 2, "call", "call" )
            , ( 3, "mail", "mail" )
            , ( 4, "mic", "mic" )
            , ( 5, "room", "room" )
            , ( 6, "send", "send" )
            , ( 7, "spellcheck", "spellcheck" )
              --, new 8 "view quit" "quit"
            ]
    in
        List.indexedMap (initBtn (360.0 / (toFloat <| List.length buttons)) x y outerRadius innerRadius) buttons
