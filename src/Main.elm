module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class)


---- MODEL ----


type alias Model =
    {count : Float}


init : ( Model, Cmd Msg )
init =
    ( {count = 0}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Frame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
          ( model, Cmd.none )
        Frame float ->
            ( {model | count = model.count + 1}, Cmd.none )




---- SUBSCRIPTIONS ----

subscriptions _ = onAnimationFrameDelta Frame

---- VIEW ----


view : Model -> Html Msg
view model =
    div [ ]
        [ h1 [] [ text "Number Counter"]
        , h1 [] [ text (String.fromFloat model.count) ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
