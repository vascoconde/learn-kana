module Kana exposing (Kana, KanaType(..), LearningKanaSelection, kanaConsonants, kanaReadings, kanaSelectionByType, reading)

import List.Extra exposing (unique)


type KanaType
    = Hiragana
    | Katakana


type alias LearningKanaSelection =
    { kanaType : KanaType
    , consonant : String
    }


type alias Kana =
    { hiragana : String
    , katakana : String
    , reading : List String -- first element in the list is the main reading
    , consonant : String
    }


kanaConsonants =
    kanaReadings
        |> List.map (\a -> a.consonant)
        |> unique


kanaSelectionByType : KanaType -> List LearningKanaSelection
kanaSelectionByType kanaType =
    kanaConsonants
        |> List.map (\consonant -> LearningKanaSelection kanaType consonant)


reading : KanaType -> Kana -> String
reading kanaType kana =
    case kanaType of
        Hiragana ->
            kana.hiragana

        Katakana ->
            kana.katakana


kanaReadings =
    [ Kana "あ" "ア" [ "a" ] ""
    , Kana "い" "イ" [ "i" ] ""
    , Kana "う" "ウ" [ "u" ] ""
    , Kana "え" "エ" [ "e" ] ""
    , Kana "お" "オ" [ "o" ] ""
    , Kana "か" "カ" [ "ka" ] "k"
    , Kana "き" "キ" [ "ki" ] "k"
    , Kana "く" "ク" [ "ku" ] "k"
    , Kana "け" "ケ" [ "ke" ] "k"
    , Kana "こ" "コ" [ "ko" ] "k"
    , Kana "さ" "サ" [ "sa" ] "s"
    , Kana "し" "シ" [ "shi", "si" ] "s"
    , Kana "す" "ス" [ "su" ] "s"
    , Kana "せ" "セ" [ "se" ] "s"
    , Kana "そ" "ソ" [ "so" ] "s"
    , Kana "た" "タ" [ "ta" ] "t"
    , Kana "ち" "チ" [ "chi", "ti" ] "t"
    , Kana "つ" "ツ" [ "tsu", "tu" ] "t"
    , Kana "て" "テ" [ "te" ] "t"
    , Kana "と" "ト" [ "to" ] "t"
    , Kana "な" "ナ" [ "na" ] "n"
    , Kana "に" "ニ" [ "ni" ] "n"
    , Kana "ぬ" "ヌ" [ "nu" ] "n"
    , Kana "ね" "ネ" [ "ne" ] "n"
    , Kana "の" "ノ" [ "no" ] "n"
    , Kana "は" "ハ" [ "ha" ] "h"
    , Kana "ひ" "ヒ" [ "hi" ] "h"
    , Kana "ふ" "フ" [ "fu", "hu" ] "h"
    , Kana "へ" "ヘ" [ "he" ] "h"
    , Kana "ほ" "ホ" [ "ho" ] "h"
    , Kana "ま" "マ" [ "ma" ] "m"
    , Kana "み" "ミ" [ "mi" ] "m"
    , Kana "む" "ム" [ "mu" ] "m"
    , Kana "め" "メ" [ "me" ] "m"
    , Kana "も" "モ" [ "mo" ] "m"
    , Kana "や" "ヤ" [ "ya" ] "y"
    , Kana "ゆ" "ユ" [ "yu" ] "y"
    , Kana "よ" "ヨ" [ "yo" ] "y"
    , Kana "ら" "ラ" [ "ra" ] "r"
    , Kana "り" "リ" [ "ri" ] "r"
    , Kana "る" "ル" [ "ru" ] "r"
    , Kana "れ" "レ" [ "re" ] "r"
    , Kana "ろ" "ロ" [ "ro" ] "r"
    , Kana "わ" "ワ" [ "wa" ] "w"
    , Kana "を" "ヲ" [ "wo" ] "w"
    , Kana "ん" "ン" [ "n" ] "nn"
    , Kana "が" "ガ" [ "ga" ] "g"
    , Kana "ぎ" "ギ" [ "gi" ] "g"
    , Kana "ぐ" "グ" [ "gu" ] "g"
    , Kana "げ" "ゲ" [ "ge" ] "g"
    , Kana "ご" "ゴ" [ "go" ] "g"
    , Kana "ざ" "ザ" [ "za" ] "z"
    , Kana "じ" "ジ" [ "ji", "zi" ] "z"
    , Kana "ず" "ズ" [ "zu" ] "z"
    , Kana "ぜ" "ゼ" [ "ze" ] "z"
    , Kana "ぞ" "ゾ" [ "zo" ] "z"
    , Kana "だ" "ダ" [ "da" ] "d"
    , Kana "ぢ" "ヂ" [ "ji", "di" ] "d"
    , Kana "づ" "ヅ" [ "zu", "du" ] "d"
    , Kana "で" "デ" [ "de" ] "d"
    , Kana "ど" "ド" [ "do" ] "d"
    , Kana "ば" "バ" [ "ba" ] "b"
    , Kana "び" "ビ" [ "bi" ] "b"
    , Kana "ぶ" "ブ" [ "bu" ] "b"
    , Kana "べ" "ベ" [ "be" ] "b"
    , Kana "ぼ" "ボ" [ "bo" ] "b"
    , Kana "ぱ" "パ" [ "pa" ] "p"
    , Kana "ぴ" "ピ" [ "pi" ] "p"
    , Kana "ぷ" "プ" [ "pu" ] "p"
    , Kana "ぺ" "ペ" [ "pe" ] "p"
    , Kana "ぽ" "ポ" [ "po" ] "p"

    --  , Kana "ぽ" "キャ" [ "kya" ] "ky"
    --  , Kana "ぽ" "キュ" [ "kyu" ] "ky"
    --  , Kana "ぽ" "キョ" [ "kyo" ] "ky"
    ]
