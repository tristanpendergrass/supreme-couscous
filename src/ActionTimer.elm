module ActionTimer exposing (ActionTimer, Timings, create, getLeft, handleAnimationFrame, isDone)


type alias Timings =
    { slideOutTime : Float
    , stayTime : Float
    , slideInTime : Float
    }


type ActionTimer
    = ActionTimer Timings Float


create : Timings -> ActionTimer
create timings =
    ActionTimer timings 0


handleAnimationFrame : Float -> ActionTimer -> ActionTimer
handleAnimationFrame delta (ActionTimer timings time) =
    ActionTimer timings (time + delta)


getLeft : ActionTimer -> Float
getLeft (ActionTimer { slideOutTime, stayTime, slideInTime } time) =
    if time < slideOutTime then
        -100 + time / slideOutTime * 100

    else if time < slideOutTime + stayTime then
        0

    else if time < slideOutTime + stayTime + slideInTime then
        (time - slideOutTime - stayTime) / slideInTime * -100

    else
        -100


isDone : ActionTimer -> Bool
isDone (ActionTimer { slideOutTime, stayTime, slideInTime } time) =
    time > slideOutTime + stayTime + slideInTime
