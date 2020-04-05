module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, br, button, div, h1, hr, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (..)
import List.Extra exposing (groupsOf)
import Random
import Random.List exposing (shuffle)
import Time



{-
   To do:

       (2) Master piping operations. |> and <| till you are completely comfortable with it.
       (3) Fix the problem of splitting lists into rows and displaying them:
       	   https://stackoverflow.com/questions/37361229/elm-split-list-into-multiple-lists
       (4) Fix the layout: we'd like everything a little more square
           Understanding bootstrap: https://medium.com/wdstack/bootstrap-equal-height-columns-d07bc934eb27
           also see here: https://stackoverflow.com/questions/19695784/how-can-i-make-bootstrap-columns-all-the-same-height#comment56504018_19695851
           https://stackoverflow.com/questions/20456694/grid-of-responsive-squares
           https://stackoverflow.com/a/49692667/4880924
           Adding a display flex will change the height stretchability of the item: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flexible_Box_Layout/Basic_Concepts_of_Flexbox i.e.  If some items are taller than others, all items will stretch along the cross axis to fill its full size.
       (5) do testing in elm
       (6) Cache the best time and display it:
       (7) Increase the font size of the buttons.


-}
---- MODEL ----


type alias Model =
    { timer : Float
    , gameState : GameState
    , currentNumber : Int
    , numbers : List Int
    }


initialModel : Model
initialModel =
    { timer = 0, gameState = NotStarted, currentNumber = 0, numbers = [] }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber)) )



---- Configuration ----


startingNumber : Int
startingNumber =
    1


endingNumber : Int
endingNumber =
    30



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix
    | ResetGame
    | NumberPress Int
    | RandomizeNumbers (List Int)


type GameState
    = NotStarted
    | Running
    | End


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | timer = model.timer + 1.0 }, Cmd.none )

        ResetGame ->
            init

        NumberPress number ->
            let
                newNumber =
                    if number == (model.currentNumber + 1) then
                        model.currentNumber + 1

                    else
                        model.currentNumber

                newSubs =
                    if model.gameState == NotStarted && number == startingNumber then
                        Running

                    else if number == endingNumber && model.currentNumber == (endingNumber - 1) then
                        End

                    else
                        model.gameState
            in
            ( { model | currentNumber = newNumber, gameState = newSubs }, Cmd.none )

        RandomizeNumbers numbers ->
            ( { model | numbers = numbers }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions model =
    if model.gameState == Running then
        Time.every 100 Tick

    else
        Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ instructions
        , showButtons model
        , timer model
        , encouragement model
        , resetButton model
        ]


encouragement : Model -> Html Msg
encouragement model =
    let
        encouragingWords =
            if model.gameState == End then
                if model.timer < 15 then
                    text "Wow!! - Excellent work!! Can you go even lower?"

                else if model.timer < 20 && model.timer >= 15 then
                    text "Nice work! Try to go sub-15?"

                else if model.timer < 30 && model.timer >= 20 then
                    text "Good effort! I think you have sub-20 potential. Wanna try again?"

                else if model.timer < 40 && model.timer >= 30 then
                    text "Good! Perhaps try for sub-30?"

                else
                    text "Nice. Try again for a faster time?"

            else
                text ""
    in
    div [ class "row" ]
        [ hr [] []
        , div [ class "col-12" ]
            [ encouragingWords ]
        ]


resetButton : Model -> Html Msg
resetButton model =
    div [ class "row" ]
        [ br [ class "row" ] []
        , hr [] []
        , if model.gameState /= NotStarted then
            button [ class "col-12 btn btn-primary", onClick ResetGame ] [ text "Reset Game" ]

          else
            text ""
        ]


instructions : Html Msg
instructions =
    div [ class "row" ]
        [ h1 [ class "col-12" ] [ text "The Number Game:" ]
        , p [ class "col-12" ] [ text ("Click from 1 through to " ++ String.fromInt endingNumber ++ " as fast as you can!") ]
        , hr [] []
        ]


timer : Model -> Html Msg
timer model =
    let
        timerString =
            String.fromFloat (model.timer / 10)

        formattedTimerString =
            if not (String.contains "." timerString) then
                timerString ++ ".0"

            else
                timerString

        addFinishComment =
            if model.gameState == End then
                " - Finished!"

            else
                ""
    in
    h1 [] [ text ("Timer: " ++ formattedTimerString ++ addFinishComment) ]


split : Int -> List a -> List (List a)
split i list =
    case take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (drop i list)


showButtons : Model -> Html Msg
showButtons model =
    div [] ((split 6 <| model.numbers) |> List.map (\x -> showButtonRow model x))


showButtonRow : Model -> List Int -> Html Msg
showButtonRow model list =
    div [ class "row no-gutters" ] (List.map (\x -> showButton model x) list)


showButton : Model -> Int -> Html Msg
showButton model buttonNumber =
    let
        highlightCurrentButton =
            if buttonNumber == model.currentNumber then
                "btn-outline-dark btn-block game-btn btn btn-danger "

            else if buttonNumber == startingNumber && model.currentNumber == 0 then
                "btn-outline-dark btn-block game-btn btn btn-success"

            else if buttonNumber < model.currentNumber then
                "btn-outline-dark btn-block game-btn btn btn-secondary"

            else
                "btn-outline-dark btn-block game-btn btn btn-light"

        obfuscateButtonNumber =
            if model.gameState == NotStarted then
                if buttonNumber /= 1 then
                    " x "

                else
                    "1"

            else
                String.fromInt buttonNumber
    in
    div [ class "col-2 d-flex justify-content-center align-items-center" ]
        [ button [ class highlightCurrentButton, onClick (NumberPress buttonNumber) ] [ text obfuscateButtonNumber ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
