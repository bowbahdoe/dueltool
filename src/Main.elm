module Main exposing (main)

import Browser
import Dict exposing (Dict)
import History exposing (History)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (append, drop, head, take)
import Maybe
import Number.Bounded exposing (Bounded)


type LifePoints
    = LifePoints (Bounded Int)


infinity : Int
infinity =
    round (1 / 0)


lpFromInt : Int -> LifePoints
lpFromInt intLife =
    LifePoints <|
        Number.Bounded.set intLife (Number.Bounded.between 0 infinity)


lpChangeBy : Int -> LifePoints -> LifePoints
lpChangeBy by (LifePoints lp) =
    LifePoints <| Number.Bounded.inc by lp


lpValue : LifePoints -> Int
lpValue (LifePoints lp) =
    Number.Bounded.value lp


lpToString : LifePoints -> String
lpToString =
    lpValue >> String.fromInt


type alias Player =
    { lifeHistory : History LifePoints
    }


type alias Model =
    { turn : Int
    , players : Dict Int Player
    }


init : Int -> Model
init numPlayers =
    { turn = 0
    , players =
        { lifeHistory = History.new (lpFromInt 8000) }
            |> List.repeat numPlayers
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    }


numberOfPlayers : Model -> Int
numberOfPlayers { players } =
    Dict.size players


getLife : Int -> Model -> Maybe Int
getLife playerNumber { players } =
    case Dict.get playerNumber players of
        Just { lifeHistory } ->
            Just (lpValue (History.current lifeHistory))

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
    { player | lifeHistory = History.to (lpChangeBy lifeChangeAmount curLife) lifeHistory }


updatePlayer : Int -> (Player -> Player) -> Model -> Model
updatePlayer playerNumber playerTransform model =
    let
        newPlayersDict =
            Dict.update playerNumber (Maybe.map playerTransform) model.players
    in
    { model | players = newPlayersDict }


type Msg
    = ChangeLife { playerNum : Int, by : Int }
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeLife { playerNum, by } ->
            updatePlayer playerNum (changeLifeBy by) model

        Reset ->
            init (numberOfPlayers model)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text
                (getLife 0 model
                    |> Maybe.withDefault 0
                    |> String.fromInt
                )
            , div [] []
            , button
                [ onClick <| ChangeLife { playerNum = 0, by = 100 } ]
                [ text "Player 1 + 100" ]
            , button
                [ onClick <| ChangeLife { playerNum = 0, by = -100 } ]
                [ text "Player 1 - 100" ]
            , div [] []
            , text
                (getLife 1 model
                    |> Maybe.withDefault 0
                    |> String.fromInt
                )
            , div [] []
            , button
                [ onClick <| ChangeLife { playerNum = 1, by = 100 } ]
                [ text "Player 2 + 100" ]
            , button
                [ onClick <| ChangeLife { playerNum = 1, by = -100 } ]
                [ text "Player 2 - 100" ]
            ]
        , div [] []
        ]


main =
    Browser.sandbox { init = init 2, view = view, update = update }
