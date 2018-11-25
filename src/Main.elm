module Main exposing (main)

import Array exposing (fromList)
import Browser
import Html exposing (Attribute, Html, button, div, h1, input, label, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Random exposing (generate)
import Random.Array exposing (sample)


kanaReadings =
    [ Kana "ア" "a" ""
    , Kana "イ" "i" ""
    , Kana "ウ" "u" ""
    , Kana "エ" "e" ""
    , Kana "オ" "o" ""
    , Kana "カ" "ka" "k"
    , Kana "キ" "ki" "k"
    , Kana "ク" "ku" "k"
    , Kana "ケ" "ke" "k"
    , Kana "コ" "ko" "k"
    , Kana "サ" "sa" "s"
    , Kana "シ" "si" "s"
    , Kana "ス" "su" "s"
    , Kana "セ" "se" "s"
    , Kana "ソ" "so" "s"
    , Kana "タ" "ta" "t"
    , Kana "チ" "ti" "t"
    , Kana "ツ" "tu" "t"
    , Kana "テ" "te" "t"
    , Kana "ト" "to" "t"
    , Kana "ナ" "na" "n"
    , Kana "ニ" "ni" "n"
    , Kana "ヌ" "nu" "n"
    , Kana "ネ" "ne" "n"
    , Kana "ノ" "no" "n"
    , Kana "ハ" "ha" "h"
    , Kana "ヒ" "hi" "h"
    , Kana "フ" "hu" "h"
    , Kana "ヘ" "he" "h"
    , Kana "ホ" "ho" "h"
    ]


kanaConsonants =
    [ "", "k", "s", "t", "n", "h" ]


type alias Kana =
    { character : String
    , reading : String
    , consonant : String
    }


type Msg
    = UpdateAnswer String
    | SubmitAnswer
    | GenerateRandomKana
    | GetRandomKana (Maybe Kana)
    | SelectKana String


type Result
    = Correct
    | Incorrect
    | None


type alias Model =
    { currentKana : Kana
    , currentAnswer : String
    , result : Result
    , numberCorrectAnswers : Int
    , numberWrongAnswers : Int
    , practicingKanaConsonants : List String
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    let
        model =
            { currentKana = Kana "" "" ""
            , currentAnswer = ""
            , result = None
            , numberCorrectAnswers = 0
            , numberWrongAnswers = 0
            , practicingKanaConsonants = kanaConsonants
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
            if model.currentAnswer == model.currentKana.reading then
                ( { model
                    | result = Correct
                    , numberCorrectAnswers = model.numberCorrectAnswers + 1
                  }
                , Random.generate GetRandomKana (randomKana model)
                )

            else if model.currentAnswer == "" then
                ( model, Cmd.none )

            else
                ( { model | result = Incorrect, currentAnswer = "", numberWrongAnswers = model.numberWrongAnswers - 1 }, Cmd.none )

        GetRandomKana maybeKana ->
            case maybeKana of
                Just kana ->
                    ( { model | currentKana = kana }, Cmd.none )

                Nothing ->
                    ( { model | currentKana = Kana "" "" "" }, Cmd.none )

        GenerateRandomKana ->
            ( model, Random.generate GetRandomKana (randomKana model) )

        SelectKana consonant ->
            let
                newModel =
                    if List.member consonant model.practicingKanaConsonants then
                        { model
                            | practicingKanaConsonants =
                                model.practicingKanaConsonants
                                    |> List.filter (\c -> c /= consonant)
                        }

                    else
                        { model
                            | practicingKanaConsonants =
                                consonant :: model.practicingKanaConsonants
                        }
            in
            ( newModel
            , Random.generate GetRandomKana (randomKana newModel)
            )


filterKanaList : List String -> List Kana -> List Kana
filterKanaList practicingKanaConsonants kanaList =
    List.filter (\kana -> List.member kana.consonant practicingKanaConsonants) kanaList


randomKana : Model -> Random.Generator (Maybe Kana)
randomKana model =
    kanaReadings
        |> filterKanaList model.practicingKanaConsonants
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


viewResultLabel : Result -> Html Msg
viewResultLabel result =
    case result of
        Correct ->
            text "👍"

        Incorrect ->
            text "👎"

        None ->
            text ""


checkbox : msg -> Bool -> Html msg
checkbox msg isChecked =
    label []
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text " "
        ]


viewKanaRow : Model -> String -> Html Msg
viewKanaRow model consonant =
    p []
        [ checkbox (SelectKana consonant) <|
            List.member consonant model.practicingKanaConsonants
        , text
            (kanaReadings
                |> List.filter (\a -> a.consonant == consonant)
                |> List.map .character
                |> String.join ""
            )
        ]


viewKanaFilters : Model -> Html Msg
viewKanaFilters model =
    div []
        [ h1 [] [ text <| "Which do you want to practice?" ]
        , div [] (List.map (\a -> viewKanaRow model a) kanaConsonants)
        ]


view : Model -> Html Msg
view model =
    div [ class "text-center font-sans" ]
        [ h1 [ class "mt-3" ] [ text <| "Learn some Kana" ]
        , div [] [ text <| "Correct: " ++ String.fromInt model.numberCorrectAnswers ++ "/" ++ String.fromInt (model.numberCorrectAnswers - model.numberWrongAnswers) ]
        , div [] [ text <| model.currentKana.character ]
        , div []
            [ input
                [ class ""
                , placeholder "Answer"
                , onInput UpdateAnswer
                , onEnter SubmitAnswer
                , value model.currentAnswer
                ]
                []
            , button [ onClick SubmitAnswer ] [ text "Submit" ]
            ]
        , div [] [ viewResultLabel model.result ]
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
