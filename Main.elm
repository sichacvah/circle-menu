module Main exposing (..)

import Html exposing (Html, div)
import Html.App as App
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Window exposing (Size)
import AnimationFrame
import Task
import Debug


-- MODEL


type alias Model =
    { size : Size
    , buttons : List Button
    }


type alias Button =
    { text : String
    }


init : Model
init =
    { size = Size 0 0
    , buttons = [ Button "search", Button "finde", Button "all", Button "fix", Button "time", Button "time" ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes SizeChange ]



-- UPDATE


type Msg
    = DoNothing
    | SizeChange Size


update : Msg -> Model -> Model
update msg model =
    case msg of
        DoNothing ->
            model

        SizeChange size ->
            { model | size = size }



-- VIEW


containerStyle : Size -> Attribute msg
containerStyle size =
    Html.Attributes.style
        [ ( "width", toString size.width ++ "px" )
        , ( "height", toString size.height ++ "px" )
        ]


type alias Cartesian =
    { x : Float
    , y : Float
    }


polarToCartesian : Float -> Float -> Float -> Float -> Cartesian
polarToCartesian cx cy r a =
    let
        shift ( x, y ) =
            { x = x + cx
            , y = y + cy
            }

        angle =
            (a - 90) * pi / 180
    in
        shift <| fromPolar ( r, angle )


describeArc : Float -> Float -> Float -> Float -> Float -> Bool -> String
describeArc x y r startAngle endAngle continueLine =
    let
        start =
            polarToCartesian x y r startAngle

        end =
            polarToCartesian x y r endAngle

        large =
            if (abs <| endAngle - startAngle) >= 180 then
                1
            else
                0

        buildString a b =
            a ++ b

        alter =
            if endAngle > startAngle then
                1
            else
                0
    in
        List.foldr
            buildString
            ""
            [ (if continueLine then
                "L"
               else
                "M"
              )
            , toString start.x
            , ","
            , toString start.y
            , " A"
            , toString r
            , ","
            , toString r
            , ",0 "
            , toString large
            , " "
            , toString alter
            , ","
            , toString end.x
            , ","
            , toString end.y
            ]


describeSector : Float -> Float -> Float -> Float -> Float -> Float -> String
describeSector x y r r2 startAngle endAngle =
    (describeArc x y r startAngle endAngle False)
        ++ " "
        ++ (describeArc x y r2 endAngle startAngle True)
        ++ " Z"


view : Model -> Html msg
view model =
    let
        size =
            500

        r1 =
            size * 0.25

        r2 =
            r1 * 0.35

        buttonsLength =
            List.length model.buttons

        angle =
            360.0 / (toFloat buttonsLength)

        c =
            (toFloat size) / 2

        svgProps =
            (SvgProps r1 r2 angle c)
    in
        svg
            [ viewBox <| "0 0 " ++ (toString size) ++ " " ++ (toString size)
            , width <| toString size
            , height <| toString size
            ]
            (List.indexedMap (renderButton svgProps) model.buttons)


type alias SvgProps =
    { r1 : Float
    , r2 : Float
    , angle : Float
    , c : Float
    }


renderButton : SvgProps -> Int -> Button -> Html msg
renderButton { r1, r2, angle, c } index button =
    Svg.g
        [Svg.Attributes.transform ("rotate(" ++ (toString <| angle * (toFloat index)) ++"," ++ (toString c) ++ "," ++ (toString c) ++ ")")]
        [ Svg.path
            [ d <| describeSector c c r1 r2 0 angle
            , class "radialnav-sector"
            ]
            []
        ]



-- [ circle
--     [ cx <| toString (size / 2)
--     , cy <| toString (size / 2)
--     , r <| toString r1
--     , radius <| toString r2
--     , fill "#fff"
--     , fillOpacity ".2"
--     , stroke "#fff"
--     , strokeWidth "1px"
--     ]
--     []
-- ]


main : Program Never
main =
    App.program
        { init = ( init, Task.perform (\_ -> DoNothing) SizeChange Window.size )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
