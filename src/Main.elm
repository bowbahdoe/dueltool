module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, height, padding, row, text, width)
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


submitLifePointChange : Model -> Model
submitLifePointChange model =
    let
        { inputState, duel } =
            model

        ( newInputState, newDuel ) =
            case InputState.lifeChangeIndicated model.inputState of
                Just { change, forPlayer } ->
                    ( inputState |> InputState.removePlayerSelection |> InputState.clearChangeInput
                    , updatePlayer forPlayer (Player.changeLifeBy change) duel
                    )

                Nothing ->
                    ( inputState, duel )
    in
    { model | inputState = newInputState, duel = newDuel }



-- Update


type Msg
    = ChangeLife { playerId : PlayerId, by : Int }
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
                text (LifePoints.toString lp)

            else
                text (LifePoints.toString lp)

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column []
            [ row [ fill |> width ]
                [ column [ fill |> width, fill |> height ]
                    [ el [ centerX ] (lifeDisplay 0 model)
                    , lifeChangeButtons 0
                    ]
                , column [ fill |> width, fill |> height ]
                    [ el [ centerX ] (lifeDisplay 1 model)
                    , lifeChangeButtons 1
                    ]
                , column []
                    [ case InputState.lifeChangeIndicated model.inputState of
                        Just { change, forPlayer } ->
                            text (String.fromInt change ++ " " ++ String.fromInt forPlayer)

                        Nothing ->
                            text "---"
                    ]
                , column []
                    [ button [] { onPress = Just (NumericButtonPressed ONE), label = text "1" }
                    , button [] { onPress = Just (NumericButtonPressed TWO), label = text "2" }
                    , button [] { onPress = Just (NumericButtonPressed THREE), label = text "3" }
                    ]
                , column []
                    [ button [] { onPress = Just (NumericButtonPressed FOUR), label = text "4" }
                    ]
                , column []
                    [ button [] { onPress = Just (NumericButtonPressed ZERO), label = text "0" }
                    , button [] { onPress = Just (NumericButtonPressed DOUBLE_ZERO), label = text "00" }
                    , button [] { onPress = Just (NumericButtonPressed TRIPLE_ZERO), label = text "000" }
                    ]
                , button [] { onPress = Just (SelectPlayer { playerId = 1 }), label = text "select player 1" }
                , button [] { onPress = Just (SelectPlayer { playerId = 2 }), label = text "select player 2" }
                ]
            , button [] { onPress = Just SubmitLifeChange, label = text "=" }
            ]


main =
    Browser.sandbox { init = init 2, view = view, update = update }
