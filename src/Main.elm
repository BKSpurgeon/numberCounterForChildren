module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, h1, hr, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (head, map, range)



{-
   To do:

       (1) Add an effect if the correct button is pressed

-}
---- MODEL ----


type alias Model =
    { count : Float
    , gameOn : Bool
    , currentNumber : Int
    }


initialModel : Model
initialModel =
    { count = 0, gameOn = False, currentNumber = 0 }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Frame Float
    | GameState
    | NumberPress Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Frame float ->
            ( { model | count = model.count + 1 }, Cmd.none )

        GameState ->
            ( initialModel, Cmd.none )

        NumberPress number ->
            let
                startGame =
                    model.currentNumber == 0 && number == 1

                newNumber =
                    if number == (model.currentNumber + 1) then
                        model.currentNumber + 1

                    else
                        model.currentNumber

                newSubs =
                    if model.gameOn == False && startGame then
                        True

                    else
                        model.gameOn
            in
            ( { model | currentNumber = newNumber, gameOn = newSubs }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions model =
    if model.gameOn then
        onAnimationFrameDelta Frame

    else
        Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        (instructions
            ++ (range 1 5 |> List.map (\x -> showButton x model.currentNumber))
            ++ [ h1 [] [ text ("Timer: " ++ String.fromFloat model.count) ]
               , hr [] []
               , if model.gameOn then
                    button [ class "btn btn-primary", onClick GameState ] [ text "Reset Game" ]

                 else
                    text ""
               ]
        )


instructions : List (Html Msg)
instructions =
    [ h1 [] [ text "The Number Game:" ]
    , p [] [ text "Click on 1 through to 50 as fast as you can!" ]
    , hr [] []
    ]


showButton : Int -> Int -> Html Msg
showButton buttonNumber currentNumber =
    let
        highlightCurrentButton =
            if buttonNumber == currentNumber then
                "btn btn-danger"

            else if buttonNumber == 1 && currentNumber == 0 then
                "btn btn-success"

            else
                "btn btn-secondary"
    in
    button [ class highlightCurrentButton, onClick (NumberPress buttonNumber) ] [ text (String.fromInt buttonNumber) ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
