module Main exposing (main)

import Array exposing (fromList)
import Browser
import Html exposing (Attribute, Html, a, br, button, div, h1, input, label, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Kana exposing (..)
import Random exposing (generate)
import Random.Array exposing (sample)


type Msg
    = UpdateAnswer String
    | SubmitAnswer
    | NextKana
    | GenerateRandomKana
    | GetRandomKana (Maybe KanaReading)
    | SelectKana LearningKanaSelection
    | SelectAllKana
    | DeselectAllKana


type Result
    = Correct
    | Incorrect
    | IncorrectShowAnswer (List String)
    | None


type alias Model =
    { currentKana : KanaReading
    , currentAnswer : String
    , result : Result
    , numberCorrectAnswers : Int
    , numberWrongAnswers : Int
    , failedAttempts : Int
    , practicingKanaConsonants : List LearningKanaSelection
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    let
        model =
            { currentKana = KanaReading "" [ "" ]
            , currentAnswer = ""
            , result = None
            , numberCorrectAnswers = 0
            , numberWrongAnswers = 0
            , failedAttempts = 0
            , practicingKanaConsonants = []
            }
    in
    ( model
    , Random.generate GetRandomKana (randomKana model)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAnswer answer ->
            ( { model | currentAnswer = answer, result = None }
            , Cmd.none
            )

        SubmitAnswer ->
            if model.currentAnswer == "" then
                ( model, Cmd.none )

            else if List.member (String.trim (String.toLower model.currentAnswer)) model.currentKana.readings then
                ( { model
                    | result = Correct
                    , currentAnswer = ""
                    , failedAttempts = 0
                    , numberCorrectAnswers = model.numberCorrectAnswers + 1
                  }
                , Random.generate GetRandomKana (randomKana model)
                )

            else if model.failedAttempts > 1 then
                ( { model
                    | result = IncorrectShowAnswer model.currentKana.readings
                    , currentAnswer = mainReading model.currentKana.readings
                    , failedAttempts = model.failedAttempts + 1
                    , numberWrongAnswers = model.numberWrongAnswers - 1
                  }
                , Cmd.none
                )

            else
                ( { model
                    | result = Incorrect
                    , currentAnswer = ""
                    , numberWrongAnswers = model.numberWrongAnswers - 1
                    , failedAttempts = model.failedAttempts + 1
                  }
                , Cmd.none
                )

        NextKana ->
            ( { model
                | currentAnswer = ""
                , failedAttempts = 0
                , result = None
              }
            , Random.generate GetRandomKana (randomKana model)
            )

        GetRandomKana maybeKana ->
            case maybeKana of
                Just kana ->
                    ( { model | currentKana = kana }, Cmd.none )

                Nothing ->
                    ( { model | currentKana = KanaReading "" [] }, Cmd.none )

        GenerateRandomKana ->
            ( model, Random.generate GetRandomKana (randomKana model) )

        SelectKana kanaSelection ->
            let
                newModel =
                    if List.member kanaSelection model.practicingKanaConsonants then
                        { model
                            | practicingKanaConsonants =
                                model.practicingKanaConsonants
                                    |> List.filter (\sel -> sel /= kanaSelection)
                        }

                    else
                        { model
                            | practicingKanaConsonants =
                                kanaSelection :: model.practicingKanaConsonants
                        }
            in
            ( newModel
            , Random.generate GetRandomKana (randomKana newModel)
            )

        SelectAllKana ->
            let
                newModel =
                    { model
                        | practicingKanaConsonants = List.concat [ kanaSelectionByType Hiragana, kanaSelectionByType Katakana ]
                    }
            in
            ( newModel
            , Random.generate GetRandomKana (randomKana newModel)
            )

        DeselectAllKana ->
            let
                newModel =
                    { model
                        | practicingKanaConsonants = []
                    }
            in
            ( newModel
            , Random.generate GetRandomKana (randomKana newModel)
            )


randomKana : Model -> Random.Generator (Maybe KanaReading)
randomKana model =
    filterKanaList model.practicingKanaConsonants
        |> Array.fromList
        |> Random.Array.sample


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


mainReading : List String -> String
mainReading readings =
    case readings of
        first :: rest ->
            first

        [] ->
            "Nothing selected"


viewResultLabel : Result -> Html Msg
viewResultLabel result =
    case result of
        Correct ->
            text "👍"

        Incorrect ->
            text "👎"

        IncorrectShowAnswer readings ->
            text ("👎 the answer is: " ++ mainReading readings)

        None ->
            text ""


checkbox : msg -> Bool -> String -> Html msg
checkbox msg isChecked labelText =
    label []
        [ input [ type_ "checkbox", checked isChecked, onClick msg, class "mr-1" ] []
        , text labelText
        ]


viewKanaRow : Model -> LearningKanaSelection -> Html Msg
viewKanaRow model kanaSelection =
    p []
        [ checkbox (SelectKana kanaSelection)
            (List.member kanaSelection model.practicingKanaConsonants)
            (kanaReadings
                |> List.filter (\a -> a.consonant == kanaSelection.consonant)
                |> List.map (reading kanaSelection.kanaType)
                |> String.join ""
            )
        ]


viewKanaFilters : Model -> Html Msg
viewKanaFilters model =
    div []
        [ h1 [ class "mt-3" ] [ text <| "Which Katakana do you want to practice?" ]
        , div []
            [ a [ onClick DeselectAllKana ] [ text "Deselect All" ]
            , span [] [ text " | " ]
            , a [ onClick SelectAllKana ] [ text "Select All" ]
            ]
        , p [] [ text "Hiragana" ]
        , div [] (List.map (\kana -> viewKanaRow model kana) (kanaSelectionByType Hiragana))
        , p [] [ text "Katakana" ]
        , div [] (List.map (\kana -> viewKanaRow model kana) (kanaSelectionByType Katakana))
        ]


submitButton : Model -> List (Html Msg)
submitButton model =
    case model.result of
        IncorrectShowAnswer stringList ->
            [ input
                [ class "shadow appearance-none block m-auto border rounded w-15 mt-2 mb-3 py-2 px-3 text-xl text-center text-grey-darker leading-tight focus:outline-none focus:shadow-outline"
                , placeholder "reading"
                , onInput UpdateAnswer
                , onEnter NextKana
                , readonly True
                , value model.currentAnswer
                ]
                []
            , button
                [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mb-4 rounded"
                , onClick NextKana
                ]
                [ text "Next" ]
            ]

        _ ->
            [ input
                [ class "shadow appearance-none block m-auto border rounded w-15 mt-2 mb-3 py-2 px-3 text-xl text-center text-grey-darker leading-tight focus:outline-none focus:shadow-outline"
                , placeholder "reading"
                , onInput UpdateAnswer
                , onEnter SubmitAnswer
                , readonly (List.isEmpty model.practicingKanaConsonants)
                , value model.currentAnswer
                ]
                []
            , button
                [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mb-4 rounded"
                , onClick SubmitAnswer
                ]
                [ text "Submit" ]
            ]


view : Model -> Html Msg
view model =
    div [ class "text-center font-sans text-gray-900 " ]
        [ h1 [ class "mt-3 text-3xl font-bold" ] [ text <| "Learn some Kana" ]
        , div [] [ text <| "Correct: " ++ String.fromInt model.numberCorrectAnswers ++ "/" ++ String.fromInt (model.numberCorrectAnswers - model.numberWrongAnswers) ]
        , if List.length model.practicingKanaConsonants > 0 then
            div [ class "text-5xl mt-3 h-12" ]
                [ text <| model.currentKana.symbol ]

          else
            div [ class "text-xl mt-3 color-gray-700 h-12" ]
                [ text <| "Select some Kana"
                , br [] []
                , text <| "to get started"
                ]
        , div [ class "h-4 mt-2" ] [ viewResultLabel model.result ]
        , div [] (submitButton model)
        , viewKanaFilters model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
