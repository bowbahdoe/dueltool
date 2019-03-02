module Die exposing (Die, Face(..), roll, value)

import Random


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type Die
    = Die Face


roll : Random.Generator Die
roll =
    Random.map Die (Random.uniform One [ Two, Three, Four, Five, Six ])


value : Die -> Int
value (Die dieFace) =
    case dieFace of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6


face : Die -> Face
face (Die dieFace) =
    dieFace
