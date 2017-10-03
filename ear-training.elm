port module EarTraining exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Random.Extra
import Dict


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

    --, incorrect : Dict.Dict (String Int)
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { currentChord = Nothing
            , chords = [ "A", "D", "E" ]
            , numberCorrect = 0

            --, incorrect = Dict.empty
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
                ( { model | numberCorrect = model.numberCorrect + 1 }, playSound ("media/correct.mp3") )
            else
                ( model, Cmd.none )

        SoundPlayed path ->
            case path of
                "media/correct.mp3" ->
                    ( model, pickNewChord model )

                _ ->
                    ( model, Cmd.none )

        NewChord newChord ->
            ( { model | currentChord = newChord }, playChord newChord )


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
    soundPlayed SoundPlayed



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text ("What chord is playing?") ]
        , div [ class "score" ]
            [ text "Number Correct: "
            , text (toString model.numberCorrect)
            , text " Out of 20"
            ]
        , div []
            (List.map
                chordButton
                model.chords
            )

        -- , audioTag model.currentChord
        ]



-- audioTag : Maybe String -> Html Msg
-- audioTag chordName =
--     case chordName of
--         Nothing ->
--             div [] []
--
--         Just value ->
--             audio [ (src ("media/" ++ value ++ "-Chord-ES-399-Piezo.mp3")), autoplay True, controls True ] []


chordButton : String -> Html Msg
chordButton chordName =
    button [ onClick (Answer chordName) ] [ text chordName ]



--- Right answer plays tone
--- wrong answer plays the currentChord again
--- button to play currentChord again
