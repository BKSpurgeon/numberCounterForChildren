port module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, br, button, div, h1, hr, img, p, small, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E
import List exposing (..)
import Random
import Random.List exposing (shuffle)
import Time


type alias Model =
    { timer : Float
    , gameState : GameState
    , currentNumber : Int
    , numbers : List Int
    , fastestTime : Maybe Float
    , currentlyPressedNumber : Int
    , gifState : GifState
    , gameMode: GameMode
    }


initialModel : Model
initialModel =
    { timer = 0
    , gameState = NotStarted
    , currentNumber = 0
    , numbers = []
    , fastestTime = Nothing
    , currentlyPressedNumber = -1
    , gifState = Failure
    , gameMode = Play
    }


init : (Json.Decode.Value) -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.Value startingValuesDecoder flags of
        Err err ->
            ( { initialModel | fastestTime = Nothing }
            , Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber))
            )

        Ok startingValue ->
            ( { initialModel | fastestTime = Just fastestTime }
            , Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber))
            )



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
    | GotGif (Result Http.Error String)
    | SetBenchmark


type GifState
    = Failure
    | Loading
    | Success String


type GameState
    = NotStarted
    | Running
    | End


type GameMode
    = Benchmark
    | Play

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | timer = model.timer + 1.0 }, Cmd.none )

        ResetGame ->
            ( { initialModel | fastestTime = model.fastestTime }
            , Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber))
            )

        NumberPress pressedNumber ->
            let
                theNextCorrectNumber =
                    if pressedNumber == (model.currentNumber + 1) then
                        model.currentNumber + 1

                    else
                        model.currentNumber

                theNewGameState =
                    if model.gameState == NotStarted && pressedNumber == startingNumber then
                        Running

                    else if pressedNumber == endingNumber && model.currentNumber == (endingNumber - 1) then
                        End

                    else
                        model.gameState

                youHaveaNewRecord fastestTime =
                    (model.timer / 10) < fastestTime
            in            
            case model.fastestTime of
                Just fastestTime ->
                    if theNewGameState == End then
                        if youHaveaNewRecord fastestTime then
                            ( { model
                                | currentlyPressedNumber = pressedNumber
                                , currentNumber = theNextCorrectNumber
                                , gameState = theNewGameState
                                , fastestTime = Just (model.timer / 10)
                              }
                            , Cmd.batch [ cacheScore (model.timer / 10), getRandomGif "victory" ]
                            )

                        else if (model.timer / 10) < 30 then
                            ( { model
                                | currentlyPressedNumber = pressedNumber
                                , currentNumber = theNextCorrectNumber
                                , gameState = theNewGameState
                              }
                            , getRandomGif "winner"
                            )

                        else
                            ( { model | currentlyPressedNumber = pressedNumber, currentNumber = theNextCorrectNumber, gameState = theNewGameState }
                            , getRandomGif "loser"
                            )

                    else
                        ( { model | currentlyPressedNumber = pressedNumber, currentNumber = theNextCorrectNumber, gameState = theNewGameState }, Cmd.none )

                Nothing ->
                    if theNewGameState == End then
                        ( { model | currentlyPressedNumber = pressedNumber, currentNumber = theNextCorrectNumber, gameState = theNewGameState, fastestTime = Just (model.timer / 10) }, Cmd.batch [ cacheScore (model.timer / 10), getRandomGif "victory" ] )
                    else  -- the game continues on: people are still playing
                        ( { model | currentlyPressedNumber = pressedNumber, currentNumber = theNextCorrectNumber, gameState = theNewGameState }, Cmd.none )

        RandomizeNumbers numbers ->
            ( { model | numbers = numbers }, Cmd.none )

        GotGif result ->
            case result of
                Ok url ->
                    ( { model | gifState = Success url }, Cmd.none )

                Err _ ->
                    ( { model | gifState = Failure }, Cmd.none )
        SetBenchmark ->
            ( { model | gameMode = Benchmark }, Cmd.none )




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
        , resetButton model
        , encouragement model
        , recordTime model
        ]


encouragement : Model -> Html Msg
encouragement model =
    let
        showappropriateGif =
            case model.gifState of
                Success url ->
                    img [ src url ] []

                _ ->
                    div [] []

        winning =
            div [ class "col-12" ]
                [ text "Wow! That's a new record! Take a screenshot as proof and see if your friends can beat this?"
                , div []
                    [ showappropriateGif ]
                ]

        encouragingWords =
            if model.gameState == End then
                case model.fastestTime of
                    Nothing ->
                        winning

                    Just fastestTime ->
                        if model.timer / 10 == fastestTime || model.timer / 10 < fastestTime then
                            winning

                        else if model.timer / 10 < 30 then
                            div [ class "col-12" ]
                                [ text "Nice time!"
                                , div []
                                    [ showappropriateGif ]
                                ]

                        else
                            div [ class "col-12" ]
                                [ text "Try again for a faster time?"
                                , div []
                                    [ showappropriateGif ]
                                ]

            else
                text ""
    in
    div [  ]
        [ encouragingWords]


resetButton : Model -> Html Msg
resetButton model =
        if model.gameMode /= Benchmark then
           div [] 
               [ button [ class "btn btn-info btn-block", onClick SetBenchmark ] [ text "Benchmark me!" ]
               , small [ class "form-text text-muted" ] [ text "Test how fast you can go: set a benchmark with all the answers shown!" ]
               ]           
        else
           button [ class "btn btn-primary btn-block", onClick ResetGame ] [ text "Reset Game!" ]

    


instructions : Html Msg
instructions =
    div [ class "row" ]
        [ h1 [ class "col-12" ] [ text "The Number Game:" ]
        , p [ class "col-12" ] [ text ("Click from 1 through to " ++ String.fromInt endingNumber ++ " as fast as you can!") ]
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
    in
    case model.gameMode of 
        Benchmark ->
            div []
            [ h1 [class "alert alert-info"] [ text ("Benchmark Game Play Mode - Timer: " ++ formattedTimerString) ]
            ]
        _ -> 
            div []
            [ h1 [] [ text ("Timer: " ++ formattedTimerString) ]
            ]


recordTime : Model -> Html Msg
recordTime model =
    let
        timerString time =
            String.fromFloat time

        formattedTimerString time =
            if not (String.contains "." (timerString time)) then
                timerString time ++ ".0"

            else
                timerString time

        fastestTimeComment =
            case model.fastestTime of
                Nothing ->
                    ""

                Just fastestTime ->
                    "(Record: " ++ formattedTimerString fastestTime ++ ")"
    in

    div []
        [ h1 [] [ text fastestTimeComment ]   
       
        ]



-- small [class "form-text text-muted"] [ text "Can you go sub 15 seconds?"]


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
        thisIsThefirstButtonToBePressed = 
            buttonNumber == startingNumber && model.currentNumber == 0

        buttonHasAlreadyBeenPressed = 
            buttonNumber < model.currentNumber

        theUserPressesTheWrongButton = 
            model.currentlyPressedNumber /= model.currentNumber && buttonNumber == model.currentlyPressedNumber

        highlightCurrentButton =
            if buttonNumber == model.currentNumber then
                "btn-block border-dark game-btn btn btn-success"

            else if thisIsThefirstButtonToBePressed then
                "btn-block border-dark game-btn btn btn-success"

            else if buttonHasAlreadyBeenPressed then
                "btn-block border-dark game-btn btn btn-secondary"

            else if theUserPressesTheWrongButton then
                "btn-block border-dark game-btn btn btn-danger"

            else
                case model.gameMode of
                    Benchmark ->
                       if buttonNumber == model.currentNumber + 1 then
                        "btn-block border-dark game-btn btn btn-primary"
                       else
                          "btn-block border-dark game-btn btn btn-light"                                               

                    _ -> "btn-block border-dark game-btn btn btn-light"

        obfuscateButtonNumber =
            case model.gameMode of
                Benchmark ->
                    String.fromInt buttonNumber
                _ ->
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


main : Program (Json.Decode.Value) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


getRandomGif : String -> Cmd Msg
getRandomGif gifType =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/random?rating=g&api_key=sd2CNhUjDfoQqLTH0nlL3Uf2YwHRHoyq&tag=" ++ gifType
        , expect = Http.expectJson GotGif gifDecoder
        }


gifDecoder : Decoder String
gifDecoder =
    field "data" (field "image_url" string)



---- Ports -----


port cacheScore : Float -> Cmd msg

port cacheBenchmark : Float -> Cmd msg -- we need to use this port otherwise dead code elimination will cut it!


 