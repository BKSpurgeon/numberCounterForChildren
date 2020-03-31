module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, text, div, h1, button, hr, p)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)


{-
To do:

    (1) Add an effect if the correct button is pressed

-}
---- MODEL ----

type alias Model =
    {count : Float
    , gameOn : Bool
    , currentNumber : Int    
    }


initialModel : Model
initialModel =
    {count = 0, gameOn = False, currentNumber = 0}

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
            ( {model | count = model.count + 1}, Cmd.none )
        GameState ->
            ( initialModel, Cmd.none )
        NumberPress number->
            let
              startGame = model.currentNumber == 0 && number == 1
              newNumber = if number == (model.currentNumber + 1)
                            then 
                                model.currentNumber + 1
                            else 
                                model.currentNumber
              newSubs     = if model.gameOn == False && startGame
                            then 
                                True
                            else 
                                model.gameOn
            in                
            ( {model | currentNumber = newNumber, gameOn = newSubs}, Cmd.none  )


---- SUBSCRIPTIONS ----

subscriptions model = 
    if model.gameOn
    then onAnimationFrameDelta Frame
    else Sub.none

    

---- VIEW ----


view : Model -> Html Msg
view model =        
    div [ ]
        [ h1 [] [text "The Number Game:"] 
        , p [] [text "Instructions: Click on 1 till the end as fast as you can! See if you can beat your friends!"]
        , button [ class "btn btn-primary", onClick GameState ] [ text "Reset Game" ]             
        , hr [] []
        , h1 [] [ text ("Current Number:" ++ String.fromInt model.currentNumber) ]   
        , h1 [] [ text ("Timer: " ++ String.fromFloat model.count)]              
        , button [ class "btn btn-danger", onClick (NumberPress 1) ] [ text "1" ]     
        , button [ class "btn btn-info", onClick (NumberPress 2) ] [ text "2" ] 
        , button [ class "btn btn-info", onClick (NumberPress 3) ] [ text "3" ] 
        , button [ class "btn btn-info", onClick (NumberPress 4) ] [ text "4" ] 
        , button [ class "btn btn-info", onClick (NumberPress 5) ] [ text "5" ] 
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
