module Button.Update exposing (..)

import Button.Model exposing (Button, Direction(..))
import Button.Msg exposing (Msg(..))
import Debug
import AnimationFrame
import Time exposing (Time, millisecond)
import Ease exposing (inOutCubic, inOutElastic, Easing)


getDirection : Bool -> Direction
getDirection isActive =
    if isActive then
        Inside
    else
        Outside


animationIsDone : Time -> Time -> Time -> Bool
animationIsDone start duration current =
    current - start >= duration


fr : Button -> Easing -> Float
fr button easing =
    let
        start =
            button.animationParams.start

        current =
            button.currentTime

        duration =
            button.animationParams.duration
    in
        easing (clamp 0 1 <| (current - start) / duration)


updateButton : Msg -> Button -> Button
updateButton msg button =
    let
        animationParams =
            button.animationParams

        updateAnimParams direction =
            { animationParams | animationIsRunning = True, animationDirection = direction, start = button.currentTime }

        toggleActivate button id isActive =
            if (button.id == id) then
                { button | active = isActive, animationParams = updateAnimParams (getDirection isActive) }
            else
                button

        tickAnim button =
            let
                delta =
                    animationParams.fromRadius - animationParams.toRadius
            in
                case button.animationParams.animationDirection of
                    Inside ->
                        if animationIsDone animationParams.start animationParams.duration button.currentTime then
                            { button
                                | outerRadius = animationParams.toRadius
                                , animationParams = { animationParams | animationIsRunning = False }
                            }
                        else
                            { button | outerRadius = animationParams.fromRadius - (fr button inOutCubic) * delta }

                    Outside ->
                        if animationIsDone animationParams.start animationParams.duration button.currentTime then
                            { button
                                | outerRadius = animationParams.fromRadius
                                , animationParams = { animationParams | animationIsRunning = False }
                            }
                        else
                            { button | outerRadius = animationParams.toRadius + (fr button inOutElastic ) * delta }
    in
        case msg of
            Activate id ->
                toggleActivate button id True

            DeActivate id ->
                toggleActivate button id False

            Tick time ->
                let
                    updatedButton =
                        { button | currentTime = time }
                in
                    if button.animationParams.animationIsRunning then
                        tickAnim updatedButton
                    else
                        updatedButton


update : Msg -> List Button -> ( List Button, Cmd Msg )
update msg buttons =
    case msg of
        Activate id ->
            ( List.map (updateButton msg) buttons, Cmd.none )

        DeActivate id ->
            ( List.map (updateButton msg) buttons, Cmd.none )

        Tick time ->
            ( List.map (updateButton msg) buttons, Cmd.none )


subscriptions : List Button -> Sub Msg
subscriptions buttons =
    if List.any (\button -> button.animationParams.animationIsRunning) buttons then
        Sub.batch [ AnimationFrame.times Tick ]
    else
        Sub.none
