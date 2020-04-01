module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, h1, hr, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (head, map, range)
import Random
import Random.List exposing (shuffle)



{-
   To do:

       (1) Add an effect if the correct button is pressed
-}
---- MODEL ----


type alias Model =
    { timer : Float
    , gameOn : Bool
    , currentNumber : Int
    , numbers : List Int
    }


initialModel : Model
initialModel =
    { timer = 0, gameOn = False, currentNumber = 0, numbers = [] }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber) ) )

---- Configuration ----

startingNumber : Int
startingNumber = 1

endingNumber : Int
endingNumber = 15

---- UPDATE ----


type Msg
    = NoOp
    | Frame Float
    | ResetGame
    | NumberPress Int
    | RandomizeNumbers (List Int)


type GameState 
    = Begin
    | Running
    | End

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Frame float ->
            ( { model | timer = model.timer + 1 }, Cmd.none )

        ResetGame ->
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
                    else if number == endingNumber && model.currentNumber == (endingNumber - 1 )then 
                        False
                    else
                        model.gameOn
            in
            ( { model | currentNumber = newNumber, gameOn = newSubs }, Cmd.none )

        RandomizeNumbers numbers ->
            ({model | numbers = numbers}, Cmd.none )



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
            ++ (model.numbers |> List.map (\x -> showButton x model.currentNumber))
            ++ [ h1 [] [ text ("Timer: " ++ String.fromFloat model.timer) ]
               , hr [] []
               , if model.currentNumber /= startingNumber - 1 then
                    button [ class "btn btn-primary", onClick ResetGame ] [ text "Reset Game" ]

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

            else if buttonNumber == startingNumber && currentNumber == 0 then
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
