module Button.View exposing (..)

import Button.Update exposing (..)
import Button.Msg exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Svg.Keyed
import Animation exposing (animate)
import Button.Msg exposing (Msg(..))
import Button.Model exposing (Button)
import Html exposing (Html)


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
            (++)
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


type alias Cartesian =
    { x : Float
    , y : Float
    }


view : List Button -> Html Msg
view buttons =
    Svg.Keyed.node
        "g"
        [ class "radialnav" ]
        (List.indexedMap renderButton buttons)


renderButton : Int -> Button -> ( String, Html Msg )
renderButton index model =
    let
        outerRadius =
            Animation.animate model.clock model.outerRadius
    in
        ( (toString index)
        , Svg.g
            [ Svg.Attributes.transform <| rotateButton model index
            , Svg.Events.onMouseOut (DeActivate model.id)
            , Svg.Events.onMouseOver (Activate model.id)
            ]
            [ Svg.path
                [ d <|
                    describeSector
                        model.x
                        model.y
                        outerRadius
                        (Animation.animate model.clock model.innerRadius)
                        0
                        model.angle
                , class <|
                    "radialnav-sector "
                        ++ (if model.state == Button.Model.Small || model.state == Button.Model.Shrinking then
                                "active"
                            else if model.state == Button.Model.Gone then
                                "hide"
                            else
                                ""
                           )
                ]
                []
            , renderIcon model outerRadius
            , renderHint model
            ]
        )


rotateButton : Button -> Int -> String
rotateButton button index =
    "rotate("
        ++ (toString <| button.angle * (toFloat index))
        ++ ","
        ++ (toString button.x)
        ++ ","
        ++ (toString button.y)
        ++ ")"


renderIcon : Button -> Float -> Html msg
renderIcon button outerRadius =
    Svg.image
        [ xlinkHref <|
            if button.state == Button.Model.Small then
                button.activeIconSrc
            else
                button.iconSrc
        , width "24"
        , height "24"
        , x << toString <| button.x - 12
        , y << toString <| button.y - 15 - outerRadius + (Animation.animate button.clock button.innerRadius)
        , class <|
            "radialnav-icon "
                ++ (if button.state == Button.Model.Gone || button.state == Button.Model.Exiting then
                        "hide"
                    else
                        ""
                   )
        , Svg.Attributes.transform ("rotate(" ++ (toString <| button.angle / 2) ++ "," ++ (toString button.x) ++ "," ++ (toString button.y) ++ ")")
        ]
        []


renderHint : Button -> Html msg
renderHint button =
    let
        id' =
            toString button.id

        pathId =
            "path-" ++ id'
    in
        Svg.text'
            [ x "0"
            , y "0"
            , class <|
                "radialnav-hint "
                    ++ if button.state == Button.Model.Small then
                        "active"
                       else if button.state == Button.Model.Here then
                        "hide"
                       else
                        ""
            ]
            [ createDefs pathId button
            , Svg.textPath
                [ xlinkHref <| "#" ++ pathId
                , Svg.Attributes.startOffset "50%"
                ]
                [ Svg.text button.hint ]
            ]


createDefs : String -> Button -> Html msg
createDefs pathId button =
    Svg.defs
        []
        [ Svg.path
            [ d <| describeArc button.x button.y (snd button.outerRadiusRange) 0 button.angle False
            , id pathId
            ]
            []
        ]
