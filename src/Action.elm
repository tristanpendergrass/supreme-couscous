module Action exposing
    ( Action
    , ActionModel(..)
    , ActionType(..)
    , Effect(..)
    , create
    , damage
    , getActionType
    , getModel
    , getStats
    , getTimer
    , isExpired
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
    = KnightModel
    | ThiefModel
    | PriestModel
    | EnemyModel


getModel : Action -> ActionModel
getModel (Action { actionType }) =
    case actionType of
        KnightAttack ->
            KnightModel

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
    case actionType of
        KnightAttack ->
            Action
                { id = id
                , timer = ActionTimer.create allyActionTimings
                , actionType = KnightAttack
                }

        ThiefAttack ->
            Action
                { id = id
                , timer = ActionTimer.create allyActionTimings
                , actionType = ThiefAttack
                }

        PriestAttack ->
            Action
                { id = id
                , timer = ActionTimer.create allyActionTimings
                , actionType = PriestAttack
                }

        EnemyAttack ->
            Action
                { id = id
                , timer = ActionTimer.create allyActionTimings
                , actionType = EnemyAttack
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
            { avatarUrl = "knight_portrait.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
            }

        ThiefAttack ->
            { avatarUrl = "thief_portrait.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
            }

        PriestAttack ->
            { avatarUrl = "priest_portrait.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
            }

        EnemyAttack ->
            { avatarUrl = "red_boy.png"
            , maxEnergy = 100
            , onSuccess = [ damage 10 ]
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
