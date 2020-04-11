port module Main exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, br, button, div, h1, hr, p, text, img, small)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Json.Encode as E
import List exposing (..)
import Random
import Random.List exposing (shuffle)
import Time

import Http
import Json.Decode exposing (Decoder, field, string)


{-
   To do:

       (2) Master piping operations. |> and <| till you are completely comfortable with it.
       (4) Fix the layout: we'd like everything a little more square
           Understanding bootstrap: https://medium.com/wdstack/bootstrap-equal-height-columns-d07bc934eb27
           also see here: https://stackoverflow.com/questions/19695784/how-can-i-make-bootstrap-columns-all-the-same-height#comment56504018_19695851
           https://stackoverflow.com/questions/20456694/grid-of-responsive-squares
           https://stackoverflow.com/a/49692667/4880924
           Adding a display flex will change the height stretchability of the item: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flexible_Box_Layout/Basic_Concepts_of_Flexbox i.e.  If some items are taller than others, all items will stretch along the cross axis to fill its full size.
       (5) do testing in elm       
       (7) Increase the font size of the buttons.
       (8) add like and subscribe buttons
       (9) add a celebration if you win.      


https://package.elm-lang.org/packages/elm/core/latest/Maybe#map
              f x =
  let
    result = Maybe.map (\v -> 3.14 > v) x
  in
    Maybe.withDefault False result


You should avoid using withDefault until you reach a point in your code where you can actually handle the Nothing case in a useful way.
    Don't withDefault to a nonsense value that you check for later. (This is a common newbie mistake when using Maybe)

-}
---- MODEL ----


type alias Model =
    { timer : Float
    , gameState : GameState
    , currentNumber : Int
    , numbers : List Int
    , fastestTime : Maybe Float
    , currentlyPressedNumber : Int
    , gifState : GifState
    }


initialModel : Model
initialModel =
    { timer = 0, gameState = NotStarted, currentNumber = 0, 
          numbers = [], fastestTime = Nothing, currentlyPressedNumber = -1, gifState = Failure }


init : Maybe Float -> ( Model, Cmd Msg )
init flags =
    case flags of
        Nothing ->
            ( { initialModel | fastestTime = Nothing }, 
                   Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber)) )

        Just fastestTime ->
            ( { initialModel | fastestTime = Just fastestTime }, 
                   Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber)) )



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


type GifState 
    = Failure
    | Loading
    | Success String


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
            ( { initialModel | fastestTime = model.fastestTime },
             Random.generate RandomizeNumbers (Random.List.shuffle (range startingNumber endingNumber))  )

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

                youHaveaNewRecord fastestTime = (model.timer / 10 ) < fastestTime 

            in
            case model.fastestTime of
                Just fastestTime ->
                    if theNewGameState == End then
                        if youHaveaNewRecord (fastestTime) then
                           ( { model | currentlyPressedNumber = pressedNumber , currentNumber = theNextCorrectNumber,
                                gameState = theNewGameState, fastestTime =  Just (model.timer / 10) }, 
                                      Cmd.batch [cacheScore (model.timer / 10), getRandomGif "victory"] )
                        else if (model.timer / 10) < 30 then
                            ( { model | currentlyPressedNumber = pressedNumber,
                                 currentNumber = theNextCorrectNumber, gameState = theNewGameState }, getRandomGif "winner" )                        
                        else
                            ( { model | currentlyPressedNumber = pressedNumber , currentNumber = theNextCorrectNumber, gameState = theNewGameState }, 
                                getRandomGif "loser" )
                    else                        
                        ( { model | currentlyPressedNumber = pressedNumber , currentNumber = theNextCorrectNumber, gameState = theNewGameState }, Cmd.none )
                Nothing ->
                    if theNewGameState == End then
                        ( { model | currentlyPressedNumber = pressedNumber , currentNumber = theNextCorrectNumber, gameState = theNewGameState, fastestTime = Just (model.timer / 10) }, Cmd.batch [cacheScore (model.timer / 10), getRandomGif "victory"]  )
                    else
                        ( { model | currentlyPressedNumber = pressedNumber , currentNumber = theNextCorrectNumber, gameState = theNewGameState }, Cmd.none )            

        RandomizeNumbers numbers ->
            ( { model | numbers = numbers }, Cmd.none )
        
        GotGif result ->
               case result of
                 Ok url ->
                   ({model | gifState = Success url}, Cmd.none)
                 Err _ ->
                   ({model | gifState = Failure}, Cmd.none)



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
        showappropriateGif = case model.gifState of
                                Success url ->
                                    img [src url] []
                                _ -> 
                                    div [] []


        winning =  div [ class "col-12" ]
                       [text "Wow! That's a new record! Take a screenshot as proof and see if your friends can beat this?"
                       , div []
                         [showappropriateGif]
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
                                [text "Nice time! How does it compare with your friends' times? Screenshot + send to them?"
                                , div []
                                      [showappropriateGif]
                                ]
                           else
                            div [ class "col-12" ]
                                [text "Try again for a faster time?"
                                , div []
                                      [showappropriateGif]
                                ]
            else
                text ""
    in
    div [ class "row" ]
        [ hr [] []
        , encouragingWords 
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

        fastestTimeComment = case model.fastestTime of
                        Nothing ->
                            ""
                        Just fastestTime ->                      
                         " (Record: " ++ String.fromFloat(fastestTime) ++ ")"
    in
    div [] [ h1 [] [ text ("Timer: " ++ formattedTimerString)]
           , br [] []  
           , h1 [] [text fastestTimeComment]
           , small [class "form-text text-muted"] [ text "Can you go sub 12 seconds? (It's possible with practice!) "]
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
        highlightCurrentButton =
            if buttonNumber == model.currentNumber then
                "btn-block border-dark game-btn btn btn-success"

            else if buttonNumber == startingNumber && model.currentNumber == 0 then
                "btn-block border-dark game-btn btn btn-success"

            else if buttonNumber < model.currentNumber then
                "btn-block border-dark game-btn btn btn-secondary"

            else if model.currentlyPressedNumber /= model.currentNumber && buttonNumber == model.currentlyPressedNumber then
                "btn-block border-dark game-btn btn btn-danger"
            else
                "btn-block border-dark game-btn btn btn-light"

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


main : Program (Maybe Float) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

----- HTTP
{-

    the number counter one:
API key: sd2CNhUjDfoQqLTH0nlL3Uf2YwHRHoyq



the test one:
    PntiBxszYGgYpLyu3SfoFaTvpG3wEoRB

-}



---- HTTP

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
port cacheScore : Float-> Cmd msg

