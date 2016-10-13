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
import Debug
import Time exposing (Time)
import AnimationFrame
import Json.Decode as Json exposing (..)
import Window exposing (Size)
import Task


-- MODEL


type alias Model =
    { buttons : List Button
    , menuShowed : Bool
    , size : Size
    }


size : Float
size =
    500


init : Model
init =
    { buttons = (initialButtons size)
    , menuShowed = False
    , size = Size 0 0
    }



-- UPDATE


type Msg
    = DoNothing
    | ButtonMsg Button.Msg.Msg
    | ToggleMenu Float Float
    | SizeChange Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonMsg buttonMsg ->
            let
                ( updatedButtons, buttonCmd ) =
                    Button.Update.update buttonMsg model.buttons
            in
                ( { model | buttons = updatedButtons }, Cmd.map ButtonMsg buttonCmd )

        ToggleMenu x y ->
            if Debug.log "SHOWED" model.menuShowed then
                let
                    ( updatedButtons, buttonCmd ) =
                        Button.Update.update (Button.Msg.HideAll) model.buttons
                in
                    ( { model | buttons = updatedButtons, menuShowed = False }, Cmd.map ButtonMsg buttonCmd )
            else
                let
                    ( updatedButtons, buttonCmd ) =
                        Button.Update.update (Button.Msg.ShowAll x y) model.buttons
                in
                    ( { model | buttons = updatedButtons, menuShowed = True }, Cmd.map ButtonMsg buttonCmd )

        SizeChange size ->
            ( { model | size = size }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ButtonMsg (Button.Update.subscriptions model.buttons)
        , Window.resizes SizeChange
        ]



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ viewBox <| "0 0 " ++ (toString model.size.width) ++ " " ++ (toString model.size.height)
        , width <| toString model.size.width
        , height <| toString model.size.height
        , class "radialnav"
        , Svg.Events.on "click" eventPos
        ]
        [ App.map ButtonMsg (Button.View.view model.buttons) ]


eventPos : Json.Decoder Msg
eventPos =
    Json.object2
        ToggleMenu
        ("clientX" := Json.float)
        ("clientY" := Json.float)


main : Program Never
main =
    App.program
        { init = ( init, Task.perform (\_ -> DoNothing) SizeChange Window.size )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
