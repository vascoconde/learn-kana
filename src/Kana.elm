module Kana exposing (Kana, kanaConsonants, kanaReadings)

import List.Extra exposing (unique)


type alias Kana =
    { character : String
    , reading : List String
    , consonant : String
    }


kanaConsonants =
    kanaReadings
        |> List.map (\a -> a.consonant)
        |> unique


kanaReadings =
    [ Kana "ア" [ "a" ] ""
    , Kana "イ" [ "i" ] ""
    , Kana "ウ" [ "u" ] ""
    , Kana "エ" [ "e" ] ""
    , Kana "オ" [ "o" ] ""
    , Kana "カ" [ "ka" ] "k"
    , Kana "キ" [ "ki" ] "k"
    , Kana "ク" [ "ku" ] "k"
    , Kana "ケ" [ "ke" ] "k"
    , Kana "コ" [ "ko" ] "k"
    , Kana "サ" [ "sa" ] "s"
    , Kana "シ" [ "shi", "si" ] "s"
    , Kana "ス" [ "su" ] "s"
    , Kana "セ" [ "se" ] "s"
    , Kana "ソ" [ "so" ] "s"
    , Kana "タ" [ "ta" ] "t"
    , Kana "チ" [ "chi", "ti" ] "t"
    , Kana "ツ" [ "tsu", "tu" ] "t"
    , Kana "テ" [ "te" ] "t"
    , Kana "ト" [ "to" ] "t"
    , Kana "ナ" [ "na" ] "n"
    , Kana "ニ" [ "ni" ] "n"
    , Kana "ヌ" [ "nu" ] "n"
    , Kana "ネ" [ "ne" ] "n"
    , Kana "ノ" [ "no" ] "n"
    , Kana "ハ" [ "ha" ] "h"
    , Kana "ヒ" [ "hi" ] "h"
    , Kana "フ" [ "fu", "hu" ] "h"
    , Kana "ヘ" [ "he" ] "h"
    , Kana "ホ" [ "ho" ] "h"
    , Kana "マ" [ "ma" ] "m"
    , Kana "ミ" [ "mi" ] "m"
    , Kana "ム" [ "mu" ] "m"
    , Kana "メ" [ "me" ] "m"
    , Kana "モ" [ "mo" ] "m"
    , Kana "ヤ" [ "ya" ] "y"
    , Kana "ユ" [ "yu" ] "y"
    , Kana "ヨ" [ "yo" ] "y"
    , Kana "ラ" [ "ra" ] "r"
    , Kana "リ" [ "ri" ] "r"
    , Kana "ル" [ "ru" ] "r"
    , Kana "レ" [ "re" ] "r"
    , Kana "ロ" [ "ro" ] "r"
    , Kana "ワ" [ "wa" ] "w"
    , Kana "ヲ" [ "wo" ] "w"
    , Kana "ン" [ "n" ] "nn"
    , Kana "ガ" [ "ga" ] "g2"
    , Kana "ギ" [ "gi" ] "g2"
    , Kana "グ" [ "gu" ] "g2"
    , Kana "ゲ" [ "ge" ] "g2"
    , Kana "ゴ" [ "go" ] "g2"
    , Kana "ザ" [ "za" ] "z2"
    , Kana "ジ" [ "ji", "zi" ] "z2"
    , Kana "ズ" [ "zu" ] "z2"
    , Kana "ゼ" [ "ze" ] "z2"
    , Kana "ゾ" [ "zo" ] "z2"
    , Kana "ダ" [ "da" ] "d2"
    , Kana "ヂ" [ "ji", "di" ] "d2"
    , Kana "ヅ" [ "zu", "du" ] "d2"
    , Kana "デ" [ "de" ] "d2"
    , Kana "ド" [ "do" ] "d2"
    , Kana "バ" [ "ba" ] "b2"
    , Kana "ビ" [ "bi" ] "b2"
    , Kana "ブ" [ "bu" ] "b2"
    , Kana "ベ" [ "be" ] "b2"
    , Kana "ボ" [ "bo" ] "b2"
    , Kana "パ" [ "pa" ] "p2"
    , Kana "ピ" [ "pi" ] "p2"
    , Kana "プ" [ "pu" ] "p2"
    , Kana "ペ" [ "pe" ] "p2"
    , Kana "ポ" [ "po" ] "p2"
    ]
