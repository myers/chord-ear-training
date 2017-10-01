module ChordEarTraining exposing (..)

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



-- MODEL


type alias Model =
    { currentChord : Maybe String
    , chords : List String
    , numberCorrect : Int
    , incorrect : Dict.Dict (String Int)
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { currentChord = Nothing
            , chords = [ "A", "E", "G" ]
            , numberCorrect = 0
            , incorrect = Dict.empty
            }
    in
        ( model
        , pickNewChord model
        )



-- UPDATE


type Msg
    = Answer String
    | NewChord (Maybe String)


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
            let
                score =
                    if Just chordName == model.currentChord then
                        model.numberCorrect + 1
                    else
                        model.numberCorrect
            in
                ( { model
                    | numberCorrect = score
                    , numberTried = model.numberTried + 1
                  }
                , pickNewChord model
                )

        NewChord newChord ->
            ( { model | currentChord = newChord }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        , audioTag model.currentChord
        ]


audioTag : Maybe String -> Html Msg
audioTag chordName =
    case chordName of
        Nothing ->
            div [] []

        Just value ->
            audio [ (src ("media/" ++ value ++ "-Chord.ogg")), autoplay True, controls True ] []


chordButton : String -> Html Msg
chordButton chordName =
    button [ onClick (Answer chordName) ] [ text chordName ]



--- Right answer plays tone
--- wrong answer plays the currentChord again
--- button to play currentChord again
