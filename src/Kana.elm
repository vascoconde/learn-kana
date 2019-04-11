module Kana exposing (Kana, kanaConsonants, kanaReadings)

import List.Extra exposing (unique)


type alias Kana =
    { hiragana : String
    , katakana : String
    , reading : List String
    , consonant : String
    }


kanaConsonants =
    kanaReadings
        |> List.map (\a -> a.consonant)
        |> unique


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
    , Kana "が" "ガ" [ "ga" ] "g2"
    , Kana "ぎ" "ギ" [ "gi" ] "g2"
    , Kana "ぐ" "グ" [ "gu" ] "g2"
    , Kana "げ" "ゲ" [ "ge" ] "g2"
    , Kana "ご" "ゴ" [ "go" ] "g2"
    , Kana "ざ" "ザ" [ "za" ] "z2"
    , Kana "じ" "ジ" [ "ji", "zi" ] "z2"
    , Kana "ず" "ズ" [ "zu" ] "z2"
    , Kana "ぜ" "ゼ" [ "ze" ] "z2"
    , Kana "ぞ" "ゾ" [ "zo" ] "z2"
    , Kana "だ" "ダ" [ "da" ] "d2"
    , Kana "ぢ" "ヂ" [ "ji", "di" ] "d2"
    , Kana "づ" "ヅ" [ "zu", "du" ] "d2"
    , Kana "で" "デ" [ "de" ] "d2"
    , Kana "ど" "ド" [ "do" ] "d2"
    , Kana "ば" "バ" [ "ba" ] "b2"
    , Kana "び" "ビ" [ "bi" ] "b2"
    , Kana "ぶ" "ブ" [ "bu" ] "b2"
    , Kana "べ" "ベ" [ "be" ] "b2"
    , Kana "ぼ" "ボ" [ "bo" ] "b2"
    , Kana "ぱ" "パ" [ "pa" ] "p2"
    , Kana "ぴ" "ピ" [ "pi" ] "p2"
    , Kana "ぷ" "プ" [ "pu" ] "p2"
    , Kana "ぺ" "ペ" [ "pe" ] "p2"
    , Kana "ぽ" "ポ" [ "po" ] "p2"
    ]
