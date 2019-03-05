module Duel exposing (Duel, decode, default, encode, getLife, updatePlayer)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LifePoints exposing (LifePoints)
import Player exposing (Player, PlayerId(..))


type alias Duel =
    ( Player, Player )


getLife : PlayerId -> Duel -> LifePoints
getLife playerId ( p1, p2 ) =
    case playerId of
        Player1 ->
            Player.life p1

        Player2 ->
            Player.life p2


{-| Transforms the player with the given id with the provided function.
-}
updatePlayer : PlayerId -> (Player -> Player) -> Duel -> Duel
updatePlayer playerId playerTransform ( p1, p2 ) =
    case playerId of
        Player1 ->
            ( playerTransform p1, p2 )

        Player2 ->
            ( p1, playerTransform p2 )


encode : Duel -> Value
encode ( p1, p2 ) =
    Encode.list Player.encode [ p1, p2 ]


decode : Decoder Duel
decode =
    Decode.list Player.decode
        |> Decode.andThen
            (\duel ->
                case duel of
                    p1 :: p2 :: [] ->
                        Decode.succeed ( p1, p2 )

                    _ ->
                        Decode.fail "Expected a duel to be stored as a list of two players."
            )


default : Duel
default =
    ( Player.withStartingLife 8000, Player.withStartingLife 8000 )
