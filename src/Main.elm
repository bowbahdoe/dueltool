module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, fillPortion, height, padding, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import InputState exposing (InputState, NumericInputButton(..))
import LifePoints exposing (LifePoints)
import Maybe
import Player exposing (Player, PlayerId)



-- Model


type alias Duel =
    { players : Dict PlayerId Player }


type alias Model =
    { duel : Duel
    , inputState : InputState
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
    if n < 100 && n /= 50 then
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
    | SelectPlayer { playerId : PlayerId }
    | RemovePlayerSelection
    | SubmitLifeChange
    | NumericButtonPressed NumericInputButton
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangeLife { playerId, by } ->
            { model | duel = updatePlayer playerId (Player.changeLifeBy by) model.duel }

        UndoLifeChange { playerId } ->
            { model | duel = updatePlayer playerId Player.undoLastLifeChange model.duel }

        RedoLifeChange { playerId } ->
            { model | duel = updatePlayer playerId Player.redoLastLifeChange model.duel }

        SelectPlayer { playerId } ->
            { model | inputState = InputState.selectPlayer playerId model.inputState }

        RemovePlayerSelection ->
            { model | inputState = InputState.removePlayerSelection model.inputState }

        SubmitLifeChange ->
            submitLifePointChange model

        NumericButtonPressed numericButton ->
            { model | inputState = InputState.pressNumeric numericButton model.inputState }

        Reset ->
            init (numberOfPlayers model.duel)



-- View


lifeChangeButtons : PlayerId -> Element Msg
lifeChangeButtons playerId =
    let
        lpChangeText : Int -> String
        lpChangeText change =
            if change >= 0 then
                "+" ++ String.fromInt change

            else
                String.fromInt change

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
            if InputState.isSelected playerId model.inputState then
                el [ Font.color (rgb255 230 0 0) ] (text (LifePoints.toString lp))

            else
                text (LifePoints.toString lp)

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
            [ inputButton NoOp "COIN"
            , inputButton SubmitLifeChange "="
            , inputButton NoOp "DICE"
            ]
        ]


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column [ width fill, height fill ]
            [ row [ fill |> width, padding 20 ]
                [ column [ fill |> width, fill |> height ]
                    [ el [ centerX, onClick (SelectPlayer { playerId = 0 }) ] (lifeDisplay 0 model)
                    ]
                , column [ fill |> width, fill |> height ]
                    [ el [ centerX, onClick (SelectPlayer { playerId = 1 }) ] (lifeDisplay 1 model)
                    ]
                ]
            , el [ centerX, padding 20 ]
                (case InputState.lifeChangeIndicated model.inputState of
                    Just { change } ->
                        text (String.fromInt change)

                    Nothing ->
                        text "---"
                )
            , numberPad
            ]


main =
    Browser.sandbox { init = init 2, view = view, update = update }
