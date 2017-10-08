-- streaks
-- % right
-- press button, play sound
-- correct button pressed, stop clock, play success, start new turn


port module EarTraining exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Random.Extra
import Dict
import Round exposing (..)
import Time exposing (..)
import AnimationFrame


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port playSound : String -> Cmd msg


port stopAndPlaySound : String -> Cmd msg


port soundPlayed : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { chords : List String
    , currentChord : Maybe String
    , turnStartTime : Maybe Time
    , turnTimeLeft : Float
    , timePerTurn : Float
    , gameLength : Int
    , correctAnswers : List String
    , incorrectAnswers : List ( String, String )
    , correctButLateAnswers : List String
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { chords = [ "D", "A", "E" ]
            , currentChord = Nothing
            , turnStartTime = Nothing
            , turnTimeLeft = 0
            , timePerTurn = 30 -- 4 seconds to answer a chord
            , gameLength = 20 -- 20 correct answers
            , correctAnswers = []
            , incorrectAnswers = []
            , correctButLateAnswers = []
            }
    in
        ( model
        , startNewTurn model
        )



-- UPDATE


type Msg
    = Answer String
    | NewTurn (Maybe String)
    | SoundPlayed String
    | PlayCurrentChord
    | CurrentTick Time



--| PlayCurrentChord


startNewTurn : Model -> Cmd Msg
startNewTurn model =
    Random.generate
        NewTurn
        (Random.Extra.filter
            (\n -> n /= model.currentChord)
            (Random.Extra.sample model.chords)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Answer chordName ->
            let
                cmds =
                    [ playChord (Just chordName) ]
            in
                case model.currentChord of
                    Nothing ->
                        ( model, Cmd.batch cmds )

                    Just currentChord ->
                        if currentChord == chordName then
                            ( { model
                                | correctAnswers = currentChord :: model.correctAnswers
                              }
                            , Cmd.batch (List.append cmds [ playSound "media/correct.mp3" ])
                            )
                        else
                            ( { model
                                | incorrectAnswers = ( currentChord, chordName ) :: model.incorrectAnswers
                              }
                            , Cmd.batch cmds
                            )

        SoundPlayed path ->
            case path of
                "media/correct.mp3" ->
                    ( model, startNewTurn model )

                _ ->
                    ( model, Cmd.none )

        NewTurn newChord ->
            ( { model
                | currentChord = newChord
                , turnStartTime = Nothing
              }
            , playChord newChord
            )

        PlayCurrentChord ->
            ( model, playChord model.currentChord )

        CurrentTick currentTime ->
            case model.turnStartTime of
                Nothing ->
                    ( { model | turnStartTime = Just currentTime, turnTimeLeft = model.timePerTurn }, Cmd.none )

                Just startTime ->
                    if turnOver model currentTime then
                        ( { model | turnTimeLeft = 0 }, Cmd.none )
                    else
                        ( { model | turnTimeLeft = (remainingTurnTime model currentTime) }, Cmd.none )


turnStarted : Model -> Bool
turnStarted model =
    model.turnStartTime == Nothing


remainingTurnTime : Model -> Time -> Float
remainingTurnTime model currentTime =
    case model.turnStartTime of
        Nothing ->
            0

        Just startTime ->
            model.timePerTurn - ((Time.inSeconds currentTime) - (Time.inSeconds startTime))


turnOver : Model -> Time -> Bool
turnOver model time =
    (remainingTurnTime model time) <= 0


playChord : Maybe String -> Cmd Msg
playChord chordName =
    case chordName of
        Nothing ->
            Cmd.none

        Just cn ->
            playSound ("media/" ++ cn ++ "-Chord-ES-399-Piezo.mp3")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ soundPlayed SoundPlayed
        , AnimationFrame.times CurrentTick
        ]



-- VIEW


isTheGameOver : Model -> Bool
isTheGameOver model =
    let
        allCorrectAnswers =
            (List.length model.correctAnswers) + (List.length model.correctButLateAnswers)
    in
        allCorrectAnswers >= model.gameLength


view : Model -> Html Msg
view model =
    if isTheGameOver model then
        gameOverScreen model
    else
        playScreen model


gameOverScreen : Model -> Html Msg
gameOverScreen model =
    div []
        [ h1 [] [ text "Game Over Man!" ]
        , div [] [ text ("You got " ++ (toString (List.length model.correctAnswers)) ++ " right") ]
        , div [] [ text (toString model.incorrectAnswers) ]
        ]


playScreen : Model -> Html Msg
playScreen model =
    div []
        [ h1 []
            [ text "What chord is that?" ]
        , div [ class "score" ]
            [ text "👍 "
            , text (toString (List.length model.correctAnswers))
            ]
        , div [ class "timer" ]
            [ text "⏱ ", text (Round.round 1 model.turnTimeLeft) ]
        , div [ class "audio-box" ]
            [ button [ class "audio", onClick PlayCurrentChord ] [ text "🔊" ] ]
        , div []
            (List.map
                chordButton
                model.chords
            )
        ]


chordButton : String -> Html Msg
chordButton chordName =
    button [ onClick (Answer chordName) ] [ text chordName ]
