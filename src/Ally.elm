module Ally exposing
    ( Ally
    , Effect(..)
    , Move
    , Stats
    , damage
    )

import Animation exposing (Animation)
import Meter exposing (Meter)


type alias Input =
    ( Char, String )


type Effect
    = Damage Int


type alias Move =
    { onSuccess : List Effect
    , prompt : String
    , inputs : List Input
    }


type alias Stats =
    { avatarUrl : String
    , battleUrl : String
    , move : Move
    , maxHealth : Int
    , maxEnergy : Int
    }


type alias Ally =
    { stats : Stats
    , health : Meter
    , energy : Meter
    , spriteAnimation : Maybe Animation
    }


damage : Int -> Effect
damage =
    Damage
