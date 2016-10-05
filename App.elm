import Html exposing (..)
import Task
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import AnimationFrame
import CircleMenu
import Json.Decode as Decode

-- MODEL

type alias Model =
  { circleMenuModel : CircleMenu.Model
  }

initialModel : Model
initialModel =
  { circleMenuModel = CircleMenu.initialModel
  }

init : ( AppModel, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- MESSAGES


type Msg
    = ToggleMenu (Int, Int)
    | CircleMenuMsg CirlceMenu.Msg

-- UPDATE

update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update message model =
  case message of
    ToggleMenu coords ->

    CircleMenuMsg subMsg ->
        let
          ( updatedCircleMenuModel, circleMenuCmd ) =
            CircleMenu.update subMsg model.circleMenuModel
        in
          ( { model | circleMenuModel = updatedCircleMenuModel }, Cmd.map CircleMenuMsg circleMenuCmd )



-- VIEW

viewStyle : Attribute msg
viewStyle =
  style [ ("backgroundColor", "#073F52") ]

view : AppModel -> Html Msg
view model =
    Html.div
        [ viewStyle
        , on "click" (decodeClickLocation ToggleMenu)
        ]
        [Html.App.map CircleMenuMsg (CircleMenuMsg.view model.circleMenuModel)
        ]


decodeClickLocation : Decode.Decoder (Int,Int)
decodeClickLocation =
    Decode.object2 (,)
        (Decode.object2 (-)
            (Decode.at ["pageX"] Decode.int)
            (Decode.at ["target", "offsetLeft"] Decode.int)
        )
        (Decode.object2 (-)
            (Decode.at ["pageY"] Decode.int)
            (Decode.at ["target", "offsetTop"] Decode.int)
        )
