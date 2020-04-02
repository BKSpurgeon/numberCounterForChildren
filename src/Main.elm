module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, h1, hr, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (head, map, range)
import Random
import Random.List exposing (shuffle)
import List.Extra exposing (groupsOf)


{-
   To do:

       (1) Fix the game state. After you finish the game you can't simply reset it.
       (2) Master piping operations. |> and <| till you are completely comfortable with it.
       (3) Fix the problem of splitting lists into rows and displaying them:
       	   https://stackoverflow.com/questions/37361229/elm-split-list-into-multiple-lists
       (4) Fix the layout: we'd like everything a little more square
           Understanding bootstrap: https://medium.com/wdstack/bootstrap-equal-height-columns-d07bc934eb27
       	   
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
endingNumber = 28


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
            init

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
            ++ [showButtons model]
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
    , p [] [ text "Click from 1 through to 50 as fast as you can!" ]
    , hr [] []
    ]

showButtons : Model -> Html Msg
showButtons model =
    div [class "container"] ( (List.Extra.groupsOf 4  <| model.numbers)  |> List.map (\x -> showButtonRow model x))


showButtonRow : Model -> List Int -> Html Msg
showButtonRow model list =
    div [class "row display-flex"] (List.map (\x -> showButton x model.currentNumber) list )  

showButton : Int -> Int -> Html Msg
showButton buttonNumber currentNumber =
    let
        highlightCurrentButton =
            if buttonNumber == currentNumber then
                "btn btn-danger col-3"

            else if buttonNumber == startingNumber && currentNumber == 0 then
                "btn btn-success col-3"

            else
                "btn btn-light col-3"
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
