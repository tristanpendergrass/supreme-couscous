module Ally exposing
    ( Ally
    , Effect(..)
    , Move
    , Stats
    , addShake
    , create
    , damage
    , drainEnergyIfFull
    , energyIsFull
    , handleAnimationFrame
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
    , recipe : List Input
    , inputs : List Input
    }


type alias Stats =
    { avatarUrl : String
    , battleUrl : String
    , tombstoneUrl : String
    , moves : List Move
    , maxEnergy : Int
    }


type alias Ally =
    { stats : Stats
    , energy : Meter
    , spriteAnimation : Maybe Animation
    }


damage : Int -> Effect
damage =
    Damage


create : Stats -> Ally
create stats =
    { stats = stats
    , energy = Meter.create (toFloat stats.maxEnergy)
    , spriteAnimation = Nothing
    }


addShake : Ally -> Ally
addShake ally =
    { ally | spriteAnimation = Just (Animation.create Animation.Shake) }


handleAnimationFrame : Float -> Ally -> Ally
handleAnimationFrame delta ally =
    { ally
        | energy = Meter.handleAnimationFrameRegen delta ally.energy
        , spriteAnimation = Maybe.andThen (Animation.updateAnimation delta) ally.spriteAnimation
    }


drainEnergyIfFull : Ally -> Ally
drainEnergyIfFull ally =
    if Meter.isFull ally.energy then
        { ally | energy = Meter.drain ally.energy }

    else
        ally


energyIsFull : Ally -> Bool
energyIsFull =
    .energy >> Meter.isFull
