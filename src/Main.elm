module Main exposing (main)

import Browser
import Browser.Events
import Coin exposing (Coin(..))
import Dict exposing (Dict)
import Die exposing (Die)
import Element exposing (Element, alpha, centerX, centerY, column, el, fill, fillPortion, height, padding, paddingXY, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import InputState exposing (InputState, NumericInputButton(..))
import Json.Encode exposing (Value)
import LifePoints exposing (LifePoints)
import Maybe
import Player exposing (Player, PlayerId)
import Random



-- Model


type alias Duel =
    { players : Dict PlayerId Player }


type DisplayableResult
    = DieRoll Die
    | CoinFlip Coin


type alias Model =
    { duel : Duel
    , inputState : InputState

    -- If the user has rolled a die or flipped a coin we want to show them the result of that
    -- action.
    , result : Maybe DisplayableResult
    }


init : Int -> Model
init numPlayers =
    { duel =
        { players =
            Player.withStartingLife 8000
                |> List.repeat numPlayers
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
        }
    , inputState = InputState.init
    , result = Nothing
    }


numberOfPlayers : Duel -> Int
numberOfPlayers duel =
    Dict.size duel.players


getLife : PlayerId -> Duel -> Maybe LifePoints
getLife playerId duel =
    case Dict.get playerId duel.players of
        Just player ->
            Just (Player.life player)

        Nothing ->
            Nothing


{-| Transforms the player with the given id with the provided function.
-}
updatePlayer : PlayerId -> (Player -> Player) -> Duel -> Duel
updatePlayer playerId playerTransform duel =
    let
        newPlayersDict =
            Dict.update playerId (Maybe.map playerTransform) duel.players
    in
    { duel | players = newPlayersDict }


{-| In YuGiOh life point changes that aren't 50 + some multiple of 100 are
almost non-existent. As a shortcut for players we can scale up a small
number to it's closest multiple of 100.
-}
scaleUpSmallLifeChanges : Int -> Int
scaleUpSmallLifeChanges n =
    if abs n < 100 && abs n /= 50 then
        n * 100

    else
        n


submitLifePointChange : Model -> Model
submitLifePointChange model =
    let
        { inputState, duel } =
            model

        ( newInputState, newDuel ) =
            case InputState.lifeChangeIndicated model.inputState of
                Just { change, forPlayer } ->
                    ( inputState |> InputState.removePlayerSelection |> InputState.clearChangeInput
                    , updatePlayer forPlayer (Player.changeLifeBy (scaleUpSmallLifeChanges change)) duel
                    )

                Nothing ->
                    ( inputState, duel )
    in
    { model | inputState = newInputState, duel = newDuel }



-- Update


type Msg
    = NoOp
    | ChangeLife { playerId : PlayerId, by : Int }
    | UndoLifeChange { playerId : PlayerId }
    | RedoLifeChange { playerId : PlayerId }
    | ToggleSelection { playerId : PlayerId }
    | SubmitLifeChange
    | NumericButtonPressed NumericInputButton
    | Reset
    | RequestDieRoll
    | RecieveDieRoll Die
    | RequestCoinFlip
    | RecieveCoinFlip Coin
    | DismissResultPrompt
    | ResizeWindow { width : Int, height : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeLife { playerId, by } ->
            ( { model | duel = updatePlayer playerId (Player.changeLifeBy by) model.duel }, Cmd.none )

        UndoLifeChange { playerId } ->
            ( { model | duel = updatePlayer playerId Player.undoLastLifeChange model.duel }, Cmd.none )

        RedoLifeChange { playerId } ->
            ( { model | duel = updatePlayer playerId Player.redoLastLifeChange model.duel }, Cmd.none )

        ToggleSelection { playerId } ->
            ( { model
                | inputState =
                    if InputState.isSelected playerId model.inputState then
                        InputState.removePlayerSelection model.inputState

                    else
                        InputState.selectPlayer playerId model.inputState
              }
            , Cmd.none
            )

        SubmitLifeChange ->
            ( submitLifePointChange model, Cmd.none )

        NumericButtonPressed numericButton ->
            ( { model | inputState = InputState.pressNumeric numericButton model.inputState }, Cmd.none )

        Reset ->
            ( init (numberOfPlayers model.duel), Cmd.none )

        RequestDieRoll ->
            ( model, Random.generate RecieveDieRoll Die.roll )

        RecieveDieRoll die ->
            ( { model | result = Just (DieRoll die) }, Cmd.none )

        RequestCoinFlip ->
            ( model, Random.generate RecieveCoinFlip Coin.flip )

        RecieveCoinFlip coin ->
            ( { model | result = Just (CoinFlip coin) }, Cmd.none )

        DismissResultPrompt ->
            ( { model | result = Nothing }, Cmd.none )

        ResizeWindow { width, height } ->
            ( model, Cmd.none )



-- View


lpChangeText : Int -> String
lpChangeText change =
    if change >= 0 then
        "+" ++ String.fromInt change

    else
        String.fromInt change


lifeChangeButtons : PlayerId -> Element Msg
lifeChangeButtons playerId =
    let
        changeLifeButton : Int -> Element Msg
        changeLifeButton changeAmt =
            button [ padding 10, centerX ]
                { onPress = Just (ChangeLife { playerId = playerId, by = changeAmt })
                , label = text (lpChangeText changeAmt)
                }
    in
    row [ width <| fill ]
        [ column [ width <| fill ]
            [ changeLifeButton 1000
            , changeLifeButton 500
            , changeLifeButton 100
            , changeLifeButton 50
            ]
        , column [ width <| fill ]
            [ changeLifeButton -1000
            , changeLifeButton -500
            , changeLifeButton -100
            , changeLifeButton -50
            ]
        ]


lifeDisplay : PlayerId -> Model -> Element Msg
lifeDisplay playerId model =
    case getLife playerId model.duel of
        Just lp ->
            let
                color =
                    if InputState.isSelected playerId model.inputState then
                        rgb255 230 0 0

                    else
                        rgb255 0 0 0
            in
            el
                [ paddingXY 30 10
                , Border.width 5
                , Border.rounded 3
                , Border.color color
                , Font.color color
                ]
                (text (LifePoints.toString lp))

        Nothing ->
            text ""


centeredText : String -> Element msg
centeredText string =
    el [ centerX ] (text string)


inputButton : msg -> String -> Element msg
inputButton msg label =
    button
        [ width <| fillPortion 1
        , height fill
        , Border.color (rgb255 100 200 0)
        , Border.solid
        , Border.width 5
        , Border.rounded 5
        , padding 10
        ]
        { onPress = Just msg, label = centeredText label }


numberPad : Element Msg
numberPad =
    column [ width fill, height fill, centerX, padding 30, Element.spacing 5 ]
        [ row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed MINUS) "-"
            , inputButton Reset "RESET"
            , inputButton (NumericButtonPressed PLUS) "+"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed ONE) "1"
            , inputButton (NumericButtonPressed TWO) "2"
            , inputButton (NumericButtonPressed THREE) "3"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed FOUR) "4"
            , inputButton (NumericButtonPressed FIVE) "5"
            , inputButton (NumericButtonPressed SIX) "6"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed SEVEN) "7"
            , inputButton (NumericButtonPressed EIGHT) "8"
            , inputButton (NumericButtonPressed NINE) "9"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed ZERO) "0"
            , inputButton (NumericButtonPressed DOUBLE_ZERO) "00"
            , inputButton (NumericButtonPressed TRIPLE_ZERO) "000"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton RequestCoinFlip "COIN"
            , inputButton SubmitLifeChange "="
            , inputButton RequestDieRoll "DICE"
            ]
        ]


resultDisplay : Model -> Element Msg
resultDisplay { result } =
    case result of
        Just res ->
            el
                [ centerX
                , centerY
                , width fill
                , height fill
                , Element.behindContent
                    (el
                        [ width fill
                        , height fill
                        , alpha 0.1
                        , Background.color (rgb255 0 25 210)
                        , onClick DismissResultPrompt
                        ]
                        Element.none
                    )
                ]
                (el
                    [ centerX
                    , centerY
                    , width (px 300)
                    , height (px 300)
                    , Background.color (rgb255 200 300 222)
                    , Font.size 128
                    ]
                    (case res of
                        DieRoll die ->
                            el [ centerX, centerY ] (Element.text (Die.toString die))

                        CoinFlip Heads ->
                            el [ centerX, centerY ] (Element.text "H")

                        CoinFlip Tails ->
                            el [ centerX, centerY ] (Element.text "T")
                    )
                )

        Nothing ->
            Element.none


view : Model -> Browser.Document Msg
view model =
    { title = "YuGiOh Duel Calculator"
    , body =
        [ Element.layout [ Element.inFront (resultDisplay model) ] <|
            column
                [ width fill
                , height fill
                ]
                [ row [ fill |> width, padding 20 ]
                    [ column [ fill |> width, fill |> height ]
                        [ el [ centerX, onClick (ToggleSelection { playerId = 0 }) ] (lifeDisplay 0 model)
                        ]
                    , column [ fill |> width, fill |> height ]
                        [ el [ centerX, onClick (ToggleSelection { playerId = 1 }) ] (lifeDisplay 1 model)
                        ]
                    ]
                , el [ centerX, padding 20 ]
                    (case InputState.lifeChangeIndicated model.inputState of
                        Just { change } ->
                            text (lpChangeText change)

                        Nothing ->
                            text "---"
                    )
                , numberPad
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\w h -> ResizeWindow { width = w, height = h })


main : Program Value Model Msg
main =
    Browser.document
        { init = \_ -> ( init 2, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
