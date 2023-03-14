port module Ports exposing (saveHiragana)


port saveHiragana :
    { level : Int
    , hiraganasVowels : Bool
    , hiraganasK : Bool
    , hiraganasS : Bool
    , hiraganasT : Bool
    , hiraganasN : Bool
    , hiraganasH : Bool
    , hiraganasM : Bool
    , hiraganasY : Bool
    , hiraganasR : Bool
    , hiraganasW : Bool
    , hiraganasSeion : Bool
    , hiraganasHandakuten : Bool
    , hiraganasDakuten : Bool
    , hiraganasYouon : Bool
    , hiraganasSokuon : Bool
    }
    -> Cmd msg
