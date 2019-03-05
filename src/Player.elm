module Player exposing (Player, PlayerId(..), changeLifeBy, life, withStartingLife, encode, decode)

import Json.Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import LifePoints exposing (LifePoints)


type Player
    = Player
        { lifePoints : LifePoints
        }


type PlayerId
    = Player1
    | Player2


withStartingLife : Int -> Player
withStartingLife lp =
    Player { lifePoints = LifePoints.fromInt lp }


changeLifeBy : Int -> Player -> Player
changeLifeBy lifeChangeAmount (Player player) =
    let
        { lifePoints } =
            player
    in
    Player { player | lifePoints = LifePoints.changeBy lifeChangeAmount lifePoints }


life : Player -> LifePoints
life (Player player) =
    player.lifePoints


encode : Player -> Value
encode (Player player) =
    Json.Encode.object [("lifePoints", LifePoints.encode player.lifePoints) ]

decode : Decoder Player
decode =
    Decode.field "lifePoints" LifePoints.decode
    |> Decode.map (\lp -> {lifePoints = lp})
    |> Decode.map Player