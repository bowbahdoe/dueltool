module Coin exposing (Coin(..), flip)

import Random


type Coin
    = Heads
    | Tails


flip : Random.Generator Coin
flip =
    Random.uniform Heads [ Tails ]
