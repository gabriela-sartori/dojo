module Main exposing (..)

import Browser
import Dict
import Element as E
import Element.Background as EB
import Element.Border as EBO
import Element.Font as EF
import Element.Input as EI
import Html.Events
import Json.Decode
import Random
import Set exposing (Set)



-- TODO: options to include dakuon and combos
-- TODO: katakana version
-- TODO: improve interface


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = E.layout [] << view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { hiraganas : List Char
    , input : String
    , error : Bool
    , corrects : Int
    , incorrects : Int
    , errors : Set Char
    , reviewing : Bool
    , level : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { hiraganas = [ 'あ' ]
      , input = ""
      , error = False
      , corrects = 0
      , incorrects = 0
      , errors = Set.empty
      , reviewing = False
      , level = 1
      }
    , generateHiraganas 1
    )


type Msg
    = GotHiraganas (List Char)
    | OnInput String
    | Submit
    | ChangeLevel
    | Review
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHiraganas hiraganas_ ->
            ( { model | hiraganas = hiraganas_ }, Cmd.none )

        OnInput str ->
            ( { model | input = str }, Cmd.none )

        Submit ->
            if matches model.hiraganas model.input then
                ( { model | error = False, input = "", corrects = model.corrects + 1 }
                , if model.reviewing then
                    generateReviewHiragana model.errors

                  else
                    generateHiraganas model.level
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
                ( model_, _ ) =
                    init

                level : Int
                level =
                    Basics.modBy 10 model.level + 1
            in
            ( { model_ | level = level }
            , generateHiraganas level
            )

        Review ->
            ( { model | reviewing = True }
            , generateReviewHiragana model.errors
            )

        Reset ->
            init


generateHiraganas : Int -> Cmd Msg
generateHiraganas size =
    let
        generator : Random.Generator Char
        generator =
            case List.map Tuple.first hiraganas of
                h :: tail ->
                    Random.uniform h tail

                _ ->
                    Random.constant '?'
    in
    generator
        |> Random.list size
        |> Random.generate GotHiraganas


generateReviewHiragana : Set Char -> Cmd Msg
generateReviewHiragana errors =
    let
        generator : Random.Generator Char
        generator =
            case Set.toList errors of
                h :: tail ->
                    Random.uniform h tail

                _ ->
                    Random.constant '?'
    in
    generator
        |> Random.map List.singleton
        |> Random.generate GotHiraganas


matches : List Char -> String -> Bool
matches hiragana input =
    hiraganaToRomaji hiragana == String.trim input


hiraganaToRomaji : List Char -> String
hiraganaToRomaji hiragana =
    hiragana
        |> List.map
            (\hiragana_ ->
                hiraganas
                    |> Dict.fromList
                    |> Dict.get hiragana_
                    |> Maybe.withDefault "?"
            )
        |> String.join ""


hiraganas : List ( Char, String )
hiraganas =
    [ [ ( 'あ', "a" ), ( 'い', "i" ), ( 'う', "u" ), ( 'え', "e" ), ( 'お', "o" ) ]
    , [ ( 'か', "ka" ), ( 'き', "ki" ), ( 'く', "ku" ), ( 'け', "ke" ), ( 'こ', "ko" ) ]
    , [ ( 'さ', "sa" ), ( 'し', "shi" ), ( 'す', "su" ), ( 'せ', "se" ), ( 'そ', "so" ) ]
    , [ ( 'た', "ta" ), ( 'ち', "chi" ), ( 'つ', "tsu" ), ( 'て', "te" ), ( 'と', "to" ) ]
    , [ ( 'な', "na" ), ( 'に', "ni" ), ( 'ぬ', "nu" ), ( 'ね', "ne" ), ( 'の', "no" ) ]
    , [ ( 'は', "ha" ), ( 'ひ', "hi" ), ( 'ふ', "fu" ), ( 'へ', "he" ), ( 'ほ', "ho" ) ]
    , [ ( 'ま', "ma" ), ( 'み', "mi" ), ( 'む', "mu" ), ( 'め', "me" ), ( 'も', "mo" ) ]
    , [ ( 'や', "ya" ), ( 'ゆ', "yu" ), ( 'よ', "yo" ) ]
    , [ ( 'ら', "ra" ), ( 'り', "ri" ), ( 'る', "ru" ), ( 'れ', "re" ), ( 'ろ', "ro" ) ]
    , [ ( 'わ', "wa" ), ( 'を', "wo" ) ]
    ]
        |> List.concat


view : Model -> E.Element Msg
view model =
    E.el [ E.paddingXY 64 32, E.centerX, EBO.rounded 8, EBO.width 1 ] <|
        E.column [ E.centerX ]
            [ E.paragraph [ EF.size 64 ]
                [ E.text (String.fromList model.hiraganas)
                ]
            , E.paragraph [ E.paddingXY 0 16 ]
                [ if model.error then
                    E.text ("Correct: " ++ hiraganaToRomaji model.hiraganas)

                  else
                    E.none
                ]
            , EI.text [ EF.size 24, onEnter Submit ]
                { onChange = OnInput
                , text = model.input
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
                viewButton ChangeLevel ("Level " ++ String.fromInt model.level)
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
              if model.level > 1 || model.reviewing || errors == 0 then
                E.none

              else
                E.el [ E.centerX, E.paddingXY 0 4 ] <|
                    viewButton Review ("Review " ++ String.fromInt errors ++ " error" ++ plural)
            , if model == Tuple.first init then
                E.none

              else
                E.el [ E.centerX, E.paddingXY 0 4 ] <|
                    viewButton Reset "Reset"
            ]


viewButton : Msg -> String -> E.Element Msg
viewButton msg label =
    EI.button [ E.centerX, E.width (E.px 200), E.height (E.px 32), EBO.rounded 4, EBO.width 1 ]
        { onPress = Just msg
        , label = E.text label
        }


onEnter : msg -> E.Attribute msg
onEnter msg =
    E.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )



-- H.div
--     []
--     [ H.p
--         [ HA.style "font-size" "64px"
--         , HA.style "line-height" "0px"
--         , HA.style "font-family" "\"Lucida Console\", \"Courier New\", monospace"
--         ]
--         [ H.text (String.fromList model.hiraganas) ]
--     , if model.error then
--         H.text ("Correct: " ++ hiraganaToRomaji model.hiraganas)
--       else
--         E.none
--     , H.form [ HE.onSubmit Submit ]
--         [ H.input [ HE.onInput OnInput, HA.value model.input ] []
--         ]
--     , if model.reviewing then
--         H.p [] []
--       else
--         H.p []
--             [ H.text
--                 ("✅ "
--                     ++ String.fromInt model.corrects
--                     ++ " ❌ "
--                     ++ String.fromInt model.incorrects
--                 )
--             ]
--     , H.div
--         [ HA.style "display" "flex"
--         , HA.style "flex-direction" "column"
--         , HA.style "gap" "4px"
--         ]
--         [ H.button [ HE.onClick ChangeLevel ]
--             [ H.text ("Level " ++ String.fromInt model.level) ]
--         , let
--             errors =
--                 Set.size model.errors
--             plural =
--                 if errors == 1 then
--                     ""
--                 else
--                     "s"
--           in
--           if model.reviewing || errors == 0 then
--             E.none
--           else
--             H.button [ HE.onClick Review ]
--                 [ H.text ("Review " ++ String.fromInt errors ++ " error" ++ plural) ]
--         , if model == Tuple.first init then
--             E.none
--           else
--             H.button [ HE.onClick Reset ] [ H.text "Reset" ]
--         ]
--     ]
