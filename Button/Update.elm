module Button.Update exposing (..)

import Button.Model exposing (Button, State(..))
import Button.Msg exposing (Msg(..))
import Debug
import AnimationFrame
import Time exposing (Time, millisecond)
import Animation exposing (..)
import Ease exposing (inOutCubic, outElastic, Easing)
import Random


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

                        Entering ->
                            if radiusDone then
                                Here
                            else
                                Entering

                        Exiting ->
                            if radiusDone then
                                Gone
                            else
                                Exiting

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

                    state =
                        model.state

                    small_r =
                        fst model.outerRadiusRange

                    big_r =
                        snd model.outerRadiusRange

                    model' =
                        { model | outerRadius = retarget now small_r model.outerRadius |> duration 200 |> ease Ease.linear, state = Shrinking }
                in
                    case model.state of
                        Here ->
                            { model | outerRadius = animation now |> from big_r |> to small_r |> duration 200 |> ease Ease.linear, state = Shrinking }

                        Growing ->
                            model'

                        Entering ->
                            { model | outerRadius = animation now |> from big_r |> to small_r |> duration 200 |> ease Ease.linear, state = Shrinking }

                        _ ->
                            model

        DeActivate id ->
            if id /= model.id then
                model
            else
                let
                    now =
                        model.clock

                    state =
                        model.state

                    growingOrBig =
                        model.state == Small || model.state == Shrinking

                    big_r =
                        snd model.outerRadiusRange

                    small_r =
                        fst model.outerRadiusRange
                in
                    case model.state of
                        Small ->
                            { model
                                | outerRadius = animation now |> from small_r |> to big_r |> duration 500 |> ease Ease.outElastic
                                , state = Growing
                            }

                        Shrinking ->
                            { model
                                | outerRadius = retarget now big_r model.outerRadius |> duration 500 |> ease Ease.outElastic
                                , state = Growing
                            }

                        _ ->
                            model

        Show id x y randomElement ->
            if id /= model.id then
                model
            else
                case model.state of
                    Gone ->
                        { model
                            | x = x
                            , y = y
                            , state = Entering
                            , outerRadius = animation model.clock |> from 0 |> to (snd model.outerRadiusRange) |> duration (800 * randomElement + 800) |> ease Ease.outElastic
                            , innerRadius = undo model.clock model.innerRadius |> duration 0
                        }

                    _ ->
                        model

        HideAll ->
            case model.state of
                Exiting ->
                    model

                _ ->
                    { model
                        | state = Exiting
                        , outerRadius = animation model.clock |> from (snd model.outerRadiusRange) |> to 0 |> duration 200
                        , innerRadius = retarget model.clock 0 model.innerRadius |> duration 200
                    }

        _ ->
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

        Show id x y randomElement ->
            ( List.map (updateButton msg) buttons, Cmd.none )

        HideAll ->
            ( List.map (updateButton msg) buttons, Cmd.none )

        ShowAll x y ->
            ( buttons, Cmd.batch (List.map (\btn -> Random.generate (Show btn.id x y) randomFloat) buttons) )


randomFloat : Random.Generator Float
randomFloat =
    Random.float 0.5 1


subscriptions : List Button -> Sub Msg
subscriptions buttons =
    Sub.batch [ AnimationFrame.diffs Tick ]
