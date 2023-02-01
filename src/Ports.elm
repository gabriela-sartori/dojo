port module Ports exposing (save)


port save :
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
    , hiraganasHandakuten : Bool
    , hiraganasDakuten : Bool
    }
    -> Cmd msg
