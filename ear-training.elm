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


port soundPlayed : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { chords : List String
    , currentChord : Maybe String
    , turnStartTime : Maybe Time
    , timePerTurn : Int
    , gameLength : Int
    , correctAnswers : List String
    , incorrectAnswers : List ( String, String )
    , lateAnswers : List String
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { chords = [ "D", "A", "E" ]
            , currentChord = Nothing
            , turnStartTime = Nothing
            , timePerTurn = 4 -- 4 seconds to answer a chord
            , gameLength = 20 -- 20 correct answers
            , correctAnswers = []
            , incorrectAnswers = []
            , lateAnswers = []
            }
    in
        ( model
        , pickNewChord model
        )



-- UPDATE


type Msg
    = Answer String
    | NewChord (Maybe String)
    | SoundPlayed String
    | PlayCurrentChord
    | CurrentTick Time



--| PlayCurrentChord


pickNewChord : Model -> Cmd Msg
pickNewChord model =
    Random.generate
        NewChord
        (Random.Extra.filter
            (\n -> n /= model.currentChord)
            (Random.Extra.sample model.chords)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Answer chordName ->
            case model.currentChord of
                Nothing ->
                    ( model, Cmd.none )

                Just currentChord ->
                    if currentChord == chordName then
                        ( { model
                            | correctAnswers = currentChord :: model.correctAnswers
                          }
                        , playSound ("media/correct.mp3")
                        )
                    else
                        ( { model
                            | incorrectAnswers = ( currentChord, chordName ) :: model.incorrectAnswers
                          }
                        , Cmd.none
                        )

        SoundPlayed path ->
            case path of
                "media/correct.mp3" ->
                    ( model, pickNewChord model )

                _ ->
                    ( model, Cmd.none )

        NewChord newChord ->
            ( { model | currentChord = newChord }, playChord newChord )

        PlayCurrentChord ->
            ( model, playChord model.currentChord )

        CurrentTick time ->
            case model.turnStartTime of
                Nothing ->
                    ( { model | turnStartTime = Just time }, Cmd.none )

                Just value ->
                    ( { model | turnTimeLeft = model.timePerTurn - ((Time.inSeconds time) - (Time.inSeconds value)) }, Cmd.none )


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
    if gameOver model then
        Sub.batch
            [ soundPlayed SoundPlayed
            , AnimationFrame.times CurrentTick
            ]
    else
        Sub.none



-- VIEW


gameOver : Model -> Bool
gameOver model =
    model.gameLength >= (List.length model.correctAnswers)


view : Model -> Html Msg
view model =
    if gameOver model then
        playScreen model
    else
        gameOverScreen model


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
            , text (toString model.numberCorrect)
            ]
        , div [ class "timer" ]
            [ text "⏱ ", text (Round.round 1 model.timeLeft) ]
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
