module Main exposing (main)

import Browser
import Dict
import Element as E
import Element.Border as EBO
import Element.Font as EF
import Element.Input as EI
import Html.Events
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Ports
import Random
import Set exposing (Set)



-- TODO: options to include dakuon and combos
-- TODO: katakana version


type alias Flags =
    { options : Maybe String }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = E.layout [] << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { hiraganas : List String
    , answer : String
    , error : Bool
    , corrects : Int
    , incorrects : Int
    , errors : Set String
    , reviewing : Bool
    , options : Options
    }


type alias Options =
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
    , hiraganasYouon : Bool
    , hiraganasSokuon : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        options : Options
        options =
            flags.options
                |> Maybe.andThen
                    (\unparsedOptions ->
                        let
                            decoder : Decoder Options
                            decoder =
                                JD.succeed Options
                                    |> JD.optional "level" JD.int 1
                                    |> JD.optional "hiraganasVowels" JD.bool False
                                    |> JD.optional "hiraganasK" JD.bool False
                                    |> JD.optional "hiraganasS" JD.bool False
                                    |> JD.optional "hiraganasT" JD.bool False
                                    |> JD.optional "hiraganasN" JD.bool False
                                    |> JD.optional "hiraganasH" JD.bool False
                                    |> JD.optional "hiraganasM" JD.bool False
                                    |> JD.optional "hiraganasY" JD.bool False
                                    |> JD.optional "hiraganasR" JD.bool False
                                    |> JD.optional "hiraganasW" JD.bool False
                                    |> JD.optional "hiraganasHandakuten" JD.bool False
                                    |> JD.optional "hiraganasDakuten" JD.bool False
                                    |> JD.optional "hiraganasYouon" JD.bool False
                                    |> JD.optional "hiraganasSokuon" JD.bool False
                        in
                        unparsedOptions
                            |> JD.decodeString decoder
                            |> Result.toMaybe
                    )
                |> Maybe.withDefault initOptions
    in
    ( { initModel | options = options }
    , generateHiraganas options
    )


initModel : Model
initModel =
    { hiraganas = []
    , answer = ""
    , error = False
    , corrects = 0
    , incorrects = 0
    , errors = Set.empty
    , reviewing = False
    , options = initOptions
    }


initOptions : Options
initOptions =
    { level = 1
    , hiraganasVowels = True
    , hiraganasK = False
    , hiraganasS = False
    , hiraganasT = False
    , hiraganasN = False
    , hiraganasH = False
    , hiraganasM = False
    , hiraganasY = False
    , hiraganasR = False
    , hiraganasW = False
    , hiraganasHandakuten = False
    , hiraganasDakuten = False
    , hiraganasYouon = False
    , hiraganasSokuon = False
    }


type Msg
    = GotHiraganas (List String)
    | OnInput InputType
    | Submit
    | ChangeLevel
    | Review
    | Reset


type InputType
    = Answer String
    | HiraganasVowels Bool
    | HiraganasK Bool
    | HiraganasS Bool
    | HiraganasT Bool
    | HiraganasN Bool
    | HiraganasH Bool
    | HiraganasM Bool
    | HiraganasY Bool
    | HiraganasR Bool
    | HiraganasW Bool
    | HiraganasHandakuten Bool
    | HiraganasDakuten Bool
    | HiraganasYouon Bool
    | HiraganasSokuon Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHiraganas hiraganas_ ->
            ( { model | hiraganas = hiraganas_ }, Cmd.none )

        OnInput inputType ->
            let
                { options } =
                    model

                newModel =
                    case inputType of
                        Answer answer ->
                            { model | answer = answer }

                        HiraganasVowels value ->
                            { model | options = { options | hiraganasVowels = value } }

                        HiraganasK value ->
                            { model | options = { options | hiraganasK = value } }

                        HiraganasS value ->
                            { model | options = { options | hiraganasS = value } }

                        HiraganasT value ->
                            { model | options = { options | hiraganasT = value } }

                        HiraganasN value ->
                            { model | options = { options | hiraganasN = value } }

                        HiraganasH value ->
                            { model | options = { options | hiraganasH = value } }

                        HiraganasM value ->
                            { model | options = { options | hiraganasM = value } }

                        HiraganasY value ->
                            { model | options = { options | hiraganasY = value } }

                        HiraganasR value ->
                            { model | options = { options | hiraganasR = value } }

                        HiraganasW value ->
                            { model | options = { options | hiraganasW = value } }

                        HiraganasHandakuten value ->
                            { model | options = { options | hiraganasHandakuten = value } }

                        HiraganasDakuten value ->
                            { model | options = { options | hiraganasDakuten = value } }

                        HiraganasYouon value ->
                            { model | options = { options | hiraganasYouon = value } }

                        HiraganasSokuon value ->
                            { model | options = { options | hiraganasSokuon = value } }

                noCharacterSelected : Bool
                noCharacterSelected =
                    let
                        o : Options
                        o =
                            newModel.options
                    in
                    [ o.hiraganasVowels
                    , o.hiraganasK
                    , o.hiraganasS
                    , o.hiraganasT
                    , o.hiraganasN
                    , o.hiraganasH
                    , o.hiraganasM
                    , o.hiraganasY
                    , o.hiraganasR
                    , o.hiraganasW
                    ]
                        |> List.all not
            in
            if noCharacterSelected then
                ( model, Cmd.none )

            else
                ( newModel
                , case inputType of
                    Answer _ ->
                        Cmd.none

                    _ ->
                        Ports.save newModel.options
                )

        Submit ->
            if matches model.hiraganas model.answer then
                ( { model | error = False, answer = "", corrects = model.corrects + 1 }
                , if model.reviewing then
                    generateReviewHiragana model.errors

                  else
                    generateHiraganas model.options
                )

            else
                ( { model
                    | error = True
                    , incorrects = model.incorrects + 1
                    , errors = List.foldl Set.insert model.errors model.hiraganas
                  }
                , Cmd.none
                )

        ChangeLevel ->
            let
                { options } =
                    model

                level : Int
                level =
                    Basics.modBy 10 options.level + 1

                newOptions =
                    { options | level = level }
            in
            ( { initModel | options = newOptions }
            , Cmd.batch
                [ generateHiraganas newOptions
                , Ports.save newOptions
                ]
            )

        Review ->
            ( { model | reviewing = True }
            , generateReviewHiragana model.errors
            )

        Reset ->
            ( { initModel | options = model.options }
            , generateHiraganas model.options
            )


generateHiraganas : Options -> Cmd Msg
generateHiraganas options =
    let
        availableHiraganas =
            options
                |> optionsToHiraganas
                |> List.map Tuple.first

        generator : Random.Generator String
        generator =
            case availableHiraganas of
                h :: tail ->
                    Random.uniform h tail

                _ ->
                    Random.constant "?"
    in
    generator
        |> Random.list options.level
        |> Random.generate GotHiraganas


generateReviewHiragana : Set String -> Cmd Msg
generateReviewHiragana errors =
    let
        generator : Random.Generator String
        generator =
            case Set.toList errors of
                h :: tail ->
                    Random.uniform h tail

                _ ->
                    Random.constant "?"
    in
    generator
        |> Random.map List.singleton
        |> Random.generate GotHiraganas


matches : List String -> String -> Bool
matches hiragana input =
    hiraganaToRomaji hiragana == String.trim input


hiraganaToRomaji : List String -> String
hiraganaToRomaji hiraganas =
    hiraganas
        |> List.map
            (\hiragana_ ->
                allHiraganas
                    |> Dict.fromList
                    |> Dict.get hiragana_
                    |> Maybe.withDefault "?"
            )
        |> String.join ""


allHiraganas : List ( String, String )
allHiraganas =
    [ hiraganasVowels
    , hiraganasK
    , hiraganasS
    , hiraganasT
    , hiraganasN
    , hiraganasH
    , hiraganasM
    , hiraganasY
    , hiraganasR
    , hiraganasW
    , hiraganasHandakuten
    , hiraganasDakutenK
    , hiraganasDakutenS
    , hiraganasDakutenT
    , hiraganasDakutenH
    , hiraganasYouonK
    , hiraganasYouonS
    , hiraganasYouonT
    , hiraganasYouonN
    , hiraganasYouonH
    , hiraganasYouonM
    , hiraganasYouonR
    , hiraganasYouonG
    , hiraganasYouonZ
    , hiraganasYouonD
    , hiraganasYouonB
    , hiraganasYouonP
    , hiraganasSokuonK
    , hiraganasSokuonS
    , hiraganasSokuonT
    , hiraganasSokuonHandakuten
    , hiraganasSokuonDakuten
    ]
        |> List.concat


optionsToHiraganas : Options -> List ( String, String )
optionsToHiraganas options =
    [ ( options.hiraganasVowels, hiraganasVowels )
    , ( options.hiraganasK, hiraganasK )
    , ( options.hiraganasS, hiraganasS )
    , ( options.hiraganasT, hiraganasT )
    , ( options.hiraganasN, hiraganasN )
    , ( options.hiraganasH, hiraganasH )
    , ( options.hiraganasM, hiraganasM )
    , ( options.hiraganasY, hiraganasY )
    , ( options.hiraganasR, hiraganasR )
    , ( options.hiraganasW, hiraganasW )
    , ( options.hiraganasHandakuten && options.hiraganasH, hiraganasHandakuten )
    , ( options.hiraganasDakuten && options.hiraganasK, hiraganasDakutenK )
    , ( options.hiraganasDakuten && options.hiraganasS, hiraganasDakutenS )
    , ( options.hiraganasDakuten && options.hiraganasT, hiraganasDakutenT )
    , ( options.hiraganasDakuten && options.hiraganasH, hiraganasDakutenH )
    , ( options.hiraganasYouon && options.hiraganasK, hiraganasYouonK )
    , ( options.hiraganasYouon && options.hiraganasS, hiraganasYouonS )
    , ( options.hiraganasYouon && options.hiraganasT, hiraganasYouonT )
    , ( options.hiraganasYouon && options.hiraganasN, hiraganasYouonN )
    , ( options.hiraganasYouon && options.hiraganasH, hiraganasYouonH )
    , ( options.hiraganasYouon && options.hiraganasM, hiraganasYouonM )
    , ( options.hiraganasYouon && options.hiraganasR, hiraganasYouonR )
    , ( options.hiraganasYouon && options.hiraganasK && options.hiraganasDakuten, hiraganasYouonG )
    , ( options.hiraganasYouon && options.hiraganasS && options.hiraganasDakuten, hiraganasYouonZ )
    , ( options.hiraganasYouon && options.hiraganasT && options.hiraganasDakuten, hiraganasYouonD )
    , ( options.hiraganasYouon && options.hiraganasH && options.hiraganasDakuten, hiraganasYouonB )
    , ( options.hiraganasYouon && options.hiraganasH && options.hiraganasHandakuten, hiraganasYouonP )
    , ( options.hiraganasSokuon && options.hiraganasK, hiraganasSokuonK )
    , ( options.hiraganasSokuon && options.hiraganasS, hiraganasSokuonS )
    , ( options.hiraganasSokuon && options.hiraganasT, hiraganasSokuonT )
    , ( options.hiraganasSokuon && options.hiraganasHandakuten, hiraganasSokuonHandakuten )
    , ( options.hiraganasSokuon && options.hiraganasDakuten, hiraganasSokuonDakuten )
    ]
        |> List.filter (\( option, _ ) -> option)
        |> List.map Tuple.second
        |> List.concat


hiraganasVowels =
    [ ( "あ", "a" )
    , ( "い", "i" )
    , ( "う", "u" )
    , ( "え", "e" )
    , ( "お", "o" )
    ]


hiraganasK =
    [ ( "か", "ka" )
    , ( "き", "ki" )
    , ( "く", "ku" )
    , ( "け", "ke" )
    , ( "こ", "ko" )
    ]


hiraganasS =
    [ ( "さ", "sa" )
    , ( "し", "shi" )
    , ( "す", "su" )
    , ( "せ", "se" )
    , ( "そ", "so" )
    ]


hiraganasT =
    [ ( "た", "ta" )
    , ( "ち", "chi" )
    , ( "つ", "tsu" )
    , ( "て", "te" )
    , ( "と", "to" )
    ]


hiraganasN =
    [ ( "な", "na" )
    , ( "に", "ni" )
    , ( "ぬ", "nu" )
    , ( "ね", "ne" )
    , ( "の", "no" )
    ]


hiraganasH =
    [ ( "は", "ha" )
    , ( "ひ", "hi" )
    , ( "ふ", "fu" )
    , ( "へ", "he" )
    , ( "ほ", "ho" )
    ]


hiraganasM =
    [ ( "ま", "ma" )
    , ( "み", "mi" )
    , ( "む", "mu" )
    , ( "め", "me" )
    , ( "も", "mo" )
    ]


hiraganasY =
    [ ( "や", "ya" )
    , ( "ゆ", "yu" )
    , ( "よ", "yo" )
    ]


hiraganasR =
    [ ( "ら", "ra" )
    , ( "り", "ri" )
    , ( "る", "ru" )
    , ( "れ", "re" )
    , ( "ろ", "ro" )
    ]


hiraganasW =
    [ ( "わ", "wa" )
    , ( "を", "wo" )
    ]


hiraganasHandakuten =
    [ ( "ぱ", "pa" )
    , ( "ぴ", "pi" )
    , ( "ぷ", "pu" )
    , ( "ぺ", "pe" )
    , ( "ぽ", "po" )
    ]


hiraganasDakutenK =
    [ ( "が", "ga" )
    , ( "ぎ", "gi" )
    , ( "ぐ", "gu" )
    , ( "げ", "ge" )
    , ( "ご", "go" )
    ]


hiraganasDakutenS =
    [ ( "ざ", "za" )
    , ( "じ", "ji" )
    , ( "ず", "zu" )
    , ( "ぜ", "ze" )
    , ( "ぞ", "zo" )
    ]


hiraganasDakutenT =
    [ ( "だ", "da" )
    , ( "ぢ", "dji" )
    , ( "づ", "dzu" )
    , ( "で", "de" )
    , ( "ど", "do" )
    ]


hiraganasDakutenH =
    [ ( "ば", "ba" )
    , ( "び", "bi" )
    , ( "ぶ", "bu" )
    , ( "べ", "be" )
    , ( "ぼ", "bo" )
    ]


hiraganasYouonK =
    [ ( "きゃ", "kya" )
    , ( "きゅ", "kyu" )
    , ( "きょ", "kyo" )
    ]


hiraganasYouonS =
    [ ( "しゃ", "sha" )
    , ( "しゅ", "shu" )
    , ( "しょ", "sho" )
    ]


hiraganasYouonT =
    [ ( "ちゃ", "cha" )
    , ( "ちゅ", "chu" )
    , ( "ちょ", "cho" )
    ]


hiraganasYouonN =
    [ ( "にゃ", "nya" )
    , ( "にゅ", "nyu" )
    , ( "にょ", "nyo" )
    ]


hiraganasYouonH =
    [ ( "ひゃ", "hya" )
    , ( "ひゅ", "hyu" )
    , ( "ひょ", "hyo" )
    ]


hiraganasYouonM =
    [ ( "みゃ", "mya" )
    , ( "みゅ", "myu" )
    , ( "みょ", "myo" )
    ]


hiraganasYouonR =
    [ ( "りゃ", "rya" )
    , ( "りゅ", "ryu" )
    , ( "りょ", "ryo" )
    ]


hiraganasYouonG =
    [ ( "ぎゃ", "gya" )
    , ( "ぎゅ", "gyu" )
    , ( "ぎょ", "gyo" )
    ]


hiraganasYouonZ =
    [ ( "じゃ", "ja" )
    , ( "じゅ", "ju" )
    , ( "じょ", "jo" )
    ]


hiraganasYouonD =
    [ ( "ぢゃ", "dya" )
    , ( "ぢゅ", "dyu" )
    , ( "ぢょ", "dyo" )
    ]


hiraganasYouonB =
    [ ( "びゃ", "bya" )
    , ( "びゅ", "byu" )
    , ( "びょ", "byo" )
    ]


hiraganasYouonP =
    [ ( "ぴゃ", "pya" )
    , ( "ぴゅ", "pyu" )
    , ( "ぴょ", "pyo" )
    ]


hiraganasSokuonK =
    [ ( "っか", "kka" )
    , ( "っき", "kki" )
    , ( "っく", "kku" )
    , ( "っけ", "kke" )
    , ( "っこ", "kko" )
    ]


hiraganasSokuonS =
    [ ( "っさ", "ssa" )
    , ( "っし", "sshi" )
    , ( "っす", "ssu" )
    , ( "っせ", "sse" )
    , ( "っそ", "sso" )
    ]


hiraganasSokuonT =
    [ ( "った", "tta" )
    , ( "っち", "tchi" )
    , ( "っつ", "ttsu" )
    , ( "って", "tte" )
    , ( "っと", "tto" )
    ]


hiraganasSokuonHandakuten =
    [ ( "っば", "bba" )
    , ( "っび", "bbi" )
    , ( "っぶ", "bbu" )
    , ( "っべ", "bbe" )
    , ( "っぼ", "bbo" )
    ]


hiraganasSokuonDakuten =
    [ ( "っぱ", "ppa" )
    , ( "っぴ", "ppi" )
    , ( "っぷ", "ppu" )
    , ( "っぺ", "ppe" )
    , ( "っぽ", "ppo" )
    ]


view : Model -> E.Element Msg
view model =
    E.row [ E.centerX, E.spacing 8 ]
        [ E.column [ E.height E.fill, E.paddingXY 64 32, E.centerX, EBO.rounded 8, EBO.width 1 ]
            [ E.paragraph [ EF.size 64 ]
                [ E.text (String.join "" model.hiraganas)
                ]
            , E.paragraph [ E.paddingXY 0 16 ]
                [ if model.error then
                    E.text ("Correct: " ++ hiraganaToRomaji model.hiraganas)

                  else
                    E.none
                ]
            , EI.text [ EF.size 24, onEnter Submit ]
                { onChange = OnInput << Answer
                , text = model.answer
                , placeholder = Nothing
                , label = EI.labelHidden "input"
                }
            , if model.reviewing then
                E.paragraph [] []

              else
                E.paragraph [ E.padding 16 ]
                    [ E.text
                        ("✅ "
                            ++ String.fromInt model.corrects
                            ++ " ❌ "
                            ++ String.fromInt model.incorrects
                        )
                    ]
            , E.el [ E.centerX, E.paddingXY 0 4 ] <|
                viewButton ChangeLevel ("Level " ++ String.fromInt model.options.level)
            , let
                errors : Int
                errors =
                    Set.size model.errors

                plural : String
                plural =
                    if errors == 1 then
                        ""

                    else
                        "s"
              in
              if model.options.level > 1 || model.reviewing || errors == 0 then
                E.none

              else
                E.el [ E.centerX, E.paddingXY 0 4 ] <|
                    viewButton Review ("Review " ++ String.fromInt errors ++ " error" ++ plural)
            , E.el [ E.centerX, E.paddingXY 0 4 ] <|
                viewButton Reset "Reset"
            ]
        , E.column [ E.height E.fill, E.paddingXY 64 32, E.spacing 12, E.centerX, EBO.rounded 8, EBO.width 1 ]
            [ E.paragraph [ E.paddingEach { bottom = 16, top = 0, left = 0, right = 0 } ] [ E.text "Options" ]
            , viewCheckbox
                { onChange = OnInput << HiraganasVowels
                , label = "Vowels"
                , checked = model.options.hiraganasVowels
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasK
                , label = "K-"
                , checked = model.options.hiraganasK
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasS
                , label = "S-"
                , checked = model.options.hiraganasS
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasT
                , label = "T-"
                , checked = model.options.hiraganasT
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasN
                , label = "N-"
                , checked = model.options.hiraganasN
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasH
                , label = "H-"
                , checked = model.options.hiraganasH
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasM
                , label = "M-"
                , checked = model.options.hiraganasM
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasY
                , label = "Y-"
                , checked = model.options.hiraganasY
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasR
                , label = "R-"
                , checked = model.options.hiraganasR
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasW
                , label = "W-"
                , checked = model.options.hiraganasW
                }
            , E.el [ E.paddingXY 0 4 ] E.none
            , viewCheckbox
                { onChange = OnInput << HiraganasHandakuten
                , label = "Handakuten ゜"
                , checked = model.options.hiraganasHandakuten
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasDakuten
                , label = "Dakuten ゛"
                , checked = model.options.hiraganasDakuten
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasYouon
                , label = "Youon ゃゅょ"
                , checked = model.options.hiraganasYouon
                }
            , viewCheckbox
                { onChange = OnInput << HiraganasSokuon
                , label = "Sokuon っ"
                , checked = model.options.hiraganasSokuon
                }
            ]
        ]


viewButton : Msg -> String -> E.Element Msg
viewButton msg label =
    EI.button [ E.centerX, E.width (E.px 200), E.height (E.px 32), EBO.rounded 4, EBO.width 1 ]
        { onPress = Just msg
        , label = E.text label
        }


viewCheckbox :
    { onChange : Bool -> Msg
    , label : String
    , checked : Bool
    }
    -> E.Element Msg
viewCheckbox { label, onChange, checked } =
    EI.checkbox []
        { onChange = onChange
        , icon = EI.defaultCheckbox
        , checked = checked
        , label = EI.labelRight [] (E.text label)
        }


onEnter : msg -> E.Attribute msg
onEnter msg =
    E.htmlAttribute
        (Html.Events.on "keyup"
            (JD.field "key" JD.string
                |> JD.andThen
                    (\key ->
                        if key == "Enter" then
                            JD.succeed msg

                        else
                            JD.fail "Not the enter key"
                    )
            )
        )
