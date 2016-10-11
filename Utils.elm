module Utils exposing (..)
import Animation
import Debug


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

describeSector' x y r r2 startAngle endAngle =
  Animation.exactly "d" (describeSector x y r r2 startAngle endAngle)

type alias Cartesian =
    { x : Float
    , y : Float
    }
