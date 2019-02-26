module LifePoints exposing (LifePoints(..), changeBy, fromInt, infinity, toString, value)

import Number.Bounded exposing (Bounded)


type LifePoints
    = LifePoints (Bounded Int)


infinity : Int
infinity =
    round (1 / 0)


fromInt : Int -> LifePoints
fromInt intLife =
    LifePoints <|
        Number.Bounded.set intLife (Number.Bounded.between 0 infinity)


changeBy : Int -> LifePoints -> LifePoints
changeBy by (LifePoints lp) =
    LifePoints <| Number.Bounded.inc by lp


value : LifePoints -> Int
value (LifePoints lp) =
    Number.Bounded.value lp


toString : LifePoints -> String
toString =
    value >> String.fromInt
