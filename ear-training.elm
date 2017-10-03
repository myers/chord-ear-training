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
    { currentChord : Maybe String
    , chords : List String
    , numberCorrect : Int
    , startTime : Maybe Time
    , timeLeft : Float
    , gameLength : Float
    , incorrectAnswers : List ( String, String )
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { currentChord = Nothing
            , chords = [ "A", "D", "E" ]
            , numberCorrect = 0
            , startTime = Nothing
            , timeLeft = 30
            , gameLength = 1
            , incorrectAnswers = []
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
            if model.currentChord == Just chordName then
                ( { model
                    | numberCorrect = model.numberCorrect + 1
                  }
                , playSound ("media/correct.mp3")
                )
            else
                case model.currentChord of
                    Nothing ->
                        ( model, Cmd.none )

                    Just currentChord ->
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
            case model.startTime of
                Nothing ->
                    ( { model | startTime = Just time }, Cmd.none )

                Just value ->
                    ( { model | timeLeft = model.gameLength - ((Time.inSeconds time) - (Time.inSeconds value)) }, Cmd.none )


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
    if model.timeLeft > 0 then
        Sub.batch
            [ soundPlayed SoundPlayed
            , AnimationFrame.times CurrentTick
            ]
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    if model.timeLeft > 0 then
        playScreen model
    else
        gameOverScreen model


gameOverScreen : Model -> Html Msg
gameOverScreen model =
    div []
        [ h1 [] [ text "Game Over Man!" ]
        , div [] [ text ("You got " ++ (toString model.numberCorrect) ++ " right") ]
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
        , div []
            [ button [ onClick PlayCurrentChord ] [ text "🔊" ] ]
        , div []
            (List.map
                chordButton
                model.chords
            )
        ]


chordButton : String -> Html Msg
chordButton chordName =
    button [ onClick (Answer chordName) ] [ text chordName ]
