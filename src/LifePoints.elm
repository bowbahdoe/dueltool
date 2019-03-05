module LifePoints exposing (LifePoints(..), changeBy, fromInt, toString, value, encode, decode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Number.Bounded exposing (Bounded)


{-| The LifePoints held by a player. Will always be within the bounds [0, inf).
-}
type LifePoints
    = LifePoints (Bounded Int)


{-| Small unexposed Int infinity. We want LifePoints to have a lower bounds of 0 but no upper bounds.
Using infinity as our upper bounds accomplishes that.
-}
infinity : Int
infinity =
    round (1 / 0)


{-| Converts the given Int to LifePoints. If the Int is below zero, this will be clipped
to zero.
-}
fromInt : Int -> LifePoints
fromInt intLife =
    LifePoints <|
        Number.Bounded.set intLife (Number.Bounded.between 0 infinity)


{-| Changes the LifePoints by the given amount. Use positive values for increases and negative
values for damage.
-}
changeBy : Int -> LifePoints -> LifePoints
changeBy by (LifePoints lp) =
    LifePoints <| Number.Bounded.inc by lp


{-| Gets the value of the LifePoints. Will always be non-negative.
-}
value : LifePoints -> Int
value (LifePoints lp) =
    Number.Bounded.value lp


{-| Converts the LifePoints to a string. This is identical to the String of the
value of the LifePoints.
-}
toString : LifePoints -> String
toString =
    value >> String.fromInt


encode : LifePoints -> Value
encode (LifePoints lp) =
    Json.Encode.int (Number.Bounded.value lp)

decode : Decoder LifePoints
decode =
    Decode.map fromInt Decode.int