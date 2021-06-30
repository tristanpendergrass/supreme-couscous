module Animation exposing
    ( Animation
    , AnimationType(..)
    , classForAnimation
    , create
    , updateAnimation
    )

import Meter exposing (Meter)


type AnimationType
    = Shake


type Animation
    = Animation AnimationType Meter


create : AnimationType -> Animation
create animationType =
    Animation animationType (Meter.createEmpty 6)


updateAnimation : Float -> Animation -> Maybe Animation
updateAnimation delta (Animation animationType meter) =
    if Meter.isFull meter then
        Nothing

    else
        Just <| Animation animationType (Meter.handleAnimationFrameRegen delta meter)


classForAnimation : Maybe Animation -> String
classForAnimation maybeAnimation =
    case maybeAnimation of
        Nothing ->
            ""

        Just (Animation Shake _) ->
            "animate-shake"
