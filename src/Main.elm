module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (append, drop, head, take)
import Maybe exposing (map)


type alias Player =
    { life : List Int
    }


setLife : Player -> Int -> Player
setLife player life =
    { life = life :: player.life
    }


getLife : List Player -> Int -> Maybe Int
getLife players playerNum =
    drop playerNum players
        |> head
        |> Maybe.andThen
            (\player ->
                case player.life of
                    life :: _ ->
                        Just life

                    [] ->
                        Nothing
            )


type alias Model =
    { turn : Int
    , players : List Player
    }


init : Int -> Model
init numPlayers =
    { turn = 0
    , players =
        List.repeat numPlayers
            { life = [ 8000 ]
            }
    }


type
    Msg
    -- (Player, Life)
    = SetLife ( Int, Maybe Int )
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetLife ( _, Nothing ) ->
            model

        SetLife ( player, Just newLife ) ->
            case head <| drop player model.players of
                Nothing ->
                    model

                Just aPlayer ->
                    { turn = model.turn + 1
                    , players =
                        append
                            (take player model.players)
                            (setLife aPlayer newLife :: drop (player + 1) model.players)
                    }

        Reset ->
            init (List.length model.players)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text (Debug.toString (getLife model.players 0))
            , div [] []
            , button
                [ onClick <| SetLife ( 0, map (\x -> x + 100) (getLife model.players 0) ) ]
                [ text "Player 1 + 100" ]
            , button
                [ onClick <| SetLife ( 0, map (\x -> x - 100) (getLife model.players 0) ) ]
                [ text "Player 1 - 100" ]
            , div [] []
            , text (Debug.toString (getLife model.players 1))
            , div [] []
            , button
                [ onClick <| SetLife ( 1, map (\x -> x + 100) (getLife model.players 1) ) ]
                [ text "Player 2 + 100" ]
            , button
                [ onClick <| SetLife ( 1, map (\x -> x - 100) (getLife model.players 1) ) ]
                [ text "Player 2 - 100" ]
            ]
        , div [] []
        ]


main =
    Browser.sandbox { init = init 2, view = view, update = update }
