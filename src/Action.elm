module Action exposing
    ( Action
    , ActionModel(..)
    , ActionType(..)
    , Effect(..)
    , KnightInput(..)
    , create
    , damage
    , getActionType
    , getModel
    , getStats
    , getTimer
    , handleKnightInput
    , isExpired
    , knightInputStats
    , mapTimer
    )

import ActionTimer exposing (ActionTimer)


type alias CommonData =
    { actionType : ActionType
    , id : Int
    , timer : ActionTimer
    }


type Action
    = Action CommonData


type ActionType
    = KnightAttack
    | ThiefAttack
    | PriestAttack
    | EnemyAttack


type ActionModel
    = KnightModel (List KnightInput)
    | ThiefModel
    | PriestModel
    | EnemyModel


getModel : Action -> ActionModel
getModel (Action { actionType }) =
    case actionType of
        KnightAttack ->
            KnightModel []

        ThiefAttack ->
            ThiefModel

        PriestAttack ->
            PriestModel

        EnemyAttack ->
            EnemyModel


getCommonData : Action -> CommonData
getCommonData (Action commonData) =
    commonData


type alias Stats =
    { avatarUrl : String
    , maxEnergy : Int
    , onSuccess : List Effect
    , bgColor : String
    }


type Effect
    = Damage Int


create : Int -> ActionType -> Action
create id actionType =
    let
        allyActionTimings : ActionTimer.Timings
        allyActionTimings =
            { slideOutTime = 500
            , stayTime = 2000
            , slideInTime = 10000
            }
    in
    Action
        { id = id
        , timer = ActionTimer.create allyActionTimings
        , actionType = actionType
        }


getActionType : Action -> ActionType
getActionType (Action { actionType }) =
    actionType


getTimer : Action -> ActionTimer
getTimer (Action { timer }) =
    timer


getStats : Action -> Stats
getStats (Action { actionType }) =
    case actionType of
        KnightAttack ->
            knightAttackStats

        ThiefAttack ->
            { avatarUrl = "thief_portrait.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
            , bgColor = "bg-yellow-800"
            }

        PriestAttack ->
            { avatarUrl = "priest_portrait.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
            , bgColor = "bg-yellow-100"
            }

        EnemyAttack ->
            { avatarUrl = "red_boy.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
            , bgColor = "bg-red-300"
            }


isExpired : Action -> Bool
isExpired =
    getCommonData >> .timer >> ActionTimer.isDone


damage : Int -> Effect
damage =
    Damage


mapTimer : (ActionTimer -> ActionTimer) -> Action -> Action
mapTimer fn action =
    case action of
        Action data ->
            let
                oldTimer =
                    data.timer
            in
            Action { data | timer = fn oldTimer }



-- Knight stuff


knightAttackStats : Stats
knightAttackStats =
    { avatarUrl = "knight_portrait.png"
    , maxEnergy = 100
    , onSuccess = [ damage 10 ]
    , bgColor = "bg-blue-500"
    }


type KnightInput
    = Slash
    | Kick
    | Wait
    | Thrust


knightInputStats : KnightInput -> { keyCode : Char, label : String }
knightInputStats knightInput =
    case knightInput of
        Slash ->
            { keyCode = 'S', label = "Slash" }

        Kick ->
            { keyCode = 'K', label = "Kick" }

        Wait ->
            { keyCode = 'W', label = "Wait" }

        Thrust ->
            { keyCode = 'T', label = "Thrust" }


handleKnightInput : KnightInput -> List KnightInput -> List KnightInput
handleKnightInput =
    (::)
