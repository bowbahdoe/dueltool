module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, padding, row, spacing, text, width)
import Element.Input exposing (button)
import History exposing (History)
import Html exposing (Html)
import LifePoints exposing (LifePoints)
import Maybe


type alias Player =
    { lifeHistory : History LifePoints
    }


type alias Model =
    { players : Dict Int Player
    }


init : Int -> Model
init numPlayers =
    { players =
        { lifeHistory = History.new (LifePoints.fromInt 8000) }
            |> List.repeat numPlayers
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    }


numberOfPlayers : Model -> Int
numberOfPlayers { players } =
    Dict.size players


getLife : PlayerId -> Model -> Maybe Int
getLife playerId { players } =
    case Dict.get playerId players of
        Just { lifeHistory } ->
            Just (LifePoints.value (History.current lifeHistory))

        Nothing ->
            Nothing


changeLifeBy : Int -> Player -> Player
changeLifeBy lifeChangeAmount player =
    let
        { lifeHistory } =
            player

        curLife =
            History.current lifeHistory
    in
    { player | lifeHistory = History.to (LifePoints.changeBy lifeChangeAmount curLife) lifeHistory }


updatePlayer : Int -> (Player -> Player) -> Model -> Model
updatePlayer playerIdber playerTransform model =
    let
        newPlayersDict =
            Dict.update playerIdber (Maybe.map playerTransform) model.players
    in
    { model | players = newPlayersDict }


type Msg
    = ChangeLife { playerId : PlayerId, by : Int }
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeLife { playerId, by } ->
            updatePlayer playerId (changeLifeBy by) model

        Reset ->
            init (numberOfPlayers model)


type alias PlayerId =
    Int


lifeButton : PlayerId -> Element Msg
lifeButton playerId =
    let
        changeLifeMsg : Int -> Msg
        changeLifeMsg changeAmt =
            ChangeLife { playerId = playerId, by = changeAmt }
    in
    row [ width <| fill ]
        [ column [ width <| fill ]
            [ button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg 1000)
                , label = text "+1000"
                }
            , button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg 500)
                , label = text "+500"
                }
            , button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg 100)
                , label = text "+100"
                }
            , button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg 50)
                , label = text "+50"
                }
            ]
        , column [ width <| fill ]
            [ button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg -1000)
                , label = text "-1000"
                }
            , button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg -500)
                , label = text "-500"
                }
            , button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg -100)
                , label = text "-100"
                }
            , button [ padding 10, centerX ]
                { onPress = Just (changeLifeMsg -50)
                , label = text "-50"
                }
            ]
        ]


lifeDisplay : Model -> PlayerId -> Element Msg
lifeDisplay model playerId =
    case getLife playerId model of
        Just lp ->
            text (String.fromInt lp)

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ fill |> width, Element.explain Debug.todo ]
            [ column [ fill |> width ]
                [ el [ centerX ] (lifeDisplay model 0)
                , lifeButton 0
                ]
            , column [ fill |> width ]
                [ el [ centerX ] (lifeDisplay model 1)
                , lifeButton 1
                ]
            ]


main =
    Browser.sandbox { init = init 2, view = view, update = update }
