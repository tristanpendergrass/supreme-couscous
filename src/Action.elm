module Action exposing
    ( Action
    , ActionModel(..)
    , ActionType(..)
    , Effect(..)
    , create
    , damage
    , getModel
    , isExpired
    , stats
    )

import ActionTimer exposing (ActionTimer)
import Html exposing (Html)


type Action
    = Action ActionType Int ActionTimer

type SelectedAction
    = SelectedAction ActionType Int ActionTimer


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

getSelectedAction : Action -> SelectedAction
getSelectedAction action =

getModel : Action -> ActionModel
getModel (Action actionType _ _) =
    case actionType of
        KnightAttack ->
            KnightModel

        ThiefAttack ->
            ThiefModel

        PriestAttack ->
            PriestModel

        EnemyAttack ->
            EnemyModel


type alias Stats =
    { avatarUrl : String
    , maxEnergy : Int
    , onSuccess : List Effect
    }


type Effect
    = Damage Int


allyActionTimings : ActionTimer.Timings
allyActionTimings =
    { slideOutTime = 500
    , stayTime = 2000
    , slideInTime = 10000
    }


create : Int -> ActionType -> Action
create id actionType =
    case actionType of
        KnightAttack ->
            { id = id
            , timer = ActionTimer.create allyActionTimings
            , actionType = KnightAttack
            }

        ThiefAttack ->
            { id = id
            , timer = ActionTimer.create allyActionTimings
            , actionType = ThiefAttack
            }

        PriestAttack ->
            { id = id
            , timer = ActionTimer.create allyActionTimings
            , actionType = PriestAttack
            }

        EnemyAttack ->
            { id = id
            , timer = ActionTimer.create allyActionTimings
            , actionType = EnemyAttack
            }


stats : ActionType -> Stats
stats actionType =
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
    .timer >> ActionTimer.isDone


damage : Int -> Effect
damage =
    Damage
