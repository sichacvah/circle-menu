module Main exposing (..)

import Html exposing (Html, div)
import Html.App as App
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Button.Model exposing (Button, initialButtons)
import Button.View
import Button.Msg
import Button.Update
import Time exposing (Time)
import AnimationFrame


-- MODEL


type alias Model =
    { buttons : List Button
    }


size : Float
size =
    500


init : Model
init =
    { buttons = (initialButtons size)
    }



-- UPDATE


type Msg
    = DoNothing
    | ButtonMsg Button.Msg.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonMsg buttonMsg ->
            let
                ( updatedButtons, buttonCmd ) =
                    Button.Update.update buttonMsg model.buttons
            in
                ( { model | buttons = updatedButtons }, Cmd.map ButtonMsg buttonCmd )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ButtonMsg (Button.Update.subscriptions model.buttons)



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ viewBox <| "0 0 " ++ (toString size) ++ " " ++ (toString size)
        , width <| toString size
        , height <| toString size
        , class "radialnav"
        ]
        [ App.map ButtonMsg (Button.View.view model.buttons) ]


main : Program Never
main =
    App.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
