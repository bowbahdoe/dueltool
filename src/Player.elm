module Player exposing (Player, PlayerId, changeLifeBy, life, redoLastLifeChange, undoLastLifeChange, withStartingLife)

import History exposing (History)
import LifePoints exposing (LifePoints)


type Player
    = Player
        { lifeHistory : History LifePoints
        }


type alias PlayerId =
    Int


withStartingLife : Int -> Player
withStartingLife lp =
    Player { lifeHistory = History.new (LifePoints.fromInt lp) }


undoLastLifeChange : Player -> Player
undoLastLifeChange (Player player) =
    Player { player | lifeHistory = History.back player.lifeHistory }


redoLastLifeChange : Player -> Player
redoLastLifeChange (Player player) =
    Player { player | lifeHistory = History.forward player.lifeHistory }


changeLifeBy : Int -> Player -> Player
changeLifeBy lifeChangeAmount (Player player) =
    let
        { lifeHistory } =
            player

        curLife =
            History.current lifeHistory
    in
    Player { player | lifeHistory = History.to (LifePoints.changeBy lifeChangeAmount curLife) lifeHistory }


life : Player -> LifePoints
life (Player player) =
    History.current player.lifeHistory
