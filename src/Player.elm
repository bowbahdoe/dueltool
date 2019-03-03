module Player exposing (Player, PlayerId(..), changeLifeBy, life, withStartingLife)

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
