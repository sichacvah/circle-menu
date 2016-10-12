module Button.Update exposing (..)

import Button.Model exposing (Button, State(..))
import Button.Msg exposing (Msg(..))
import Debug
import AnimationFrame
import Time exposing (Time, millisecond)
import Animation exposing (..)
import Ease exposing (inOutCubic, outElastic, Easing)


updateButton : Msg -> Button -> Button
updateButton msg model =
    case msg of
        Tick t ->
            let
                clock =
                    model.clock + t

                radiusDone =
                    isDone clock model.outerRadius

                state =
                    case model.state of
                        Growing ->
                            if radiusDone then
                                Here
                            else
                                Growing

                        Shrinking ->
                            if radiusDone then
                                Small
                            else
                                Shrinking

                        _ ->
                            model.state
            in
                { model | clock = clock, state = state }

        Activate id ->
            if id /= model.id then
                model
            else
                let
                    now =
                        model.clock

                    small_r =
                        fst model.outerRadiusRange
                in
                    if model.state == Here then
                        { model
                            | outerRadius = retarget now small_r model.outerRadius |> duration 200
                            , state = Shrinking
                        }
                    else
                        model

        DeActivate id ->
            if id /= model.id then
                model
            else
                let
                    now =
                        model.clock

                    growingOrBig =
                        model.state == Small || model.state == Shrinking

                    big_r =
                        snd model.outerRadiusRange
                in
                    if growingOrBig then
                        { model
                            | outerRadius = undo now model.outerRadius |> duration 500 |> ease outElastic
                            , state = Growing
                        }
                    else
                        model


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
    Sub.batch [ AnimationFrame.diffs Tick ]
