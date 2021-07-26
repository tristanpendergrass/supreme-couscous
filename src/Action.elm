module Action exposing
    ( Action
    , ActionModel(..)
    , ActionType(..)
    , Effect(..)
    , SelectedAction
    , create
    , damage
    , getActionType
    , getStats
    , isExpired
    , select
    , unselect
    )

import ActionTimer exposing (ActionTimer)


type Action
    = Action ActionType Int ActionTimer


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


select : Action -> SelectedAction
select action =
    let
        commonData =
            getCommonData action
    in
    case commonData.actionType of
        KnightAttack ->
            SelectedAction commonData KnightModel

        ThiefAttack ->
            SelectedAction commonData ThiefModel

        PriestAttack ->
            SelectedAction commonData PriestModel

        EnemyAttack ->
            SelectedAction commonData EnemyModel


unselect : SelectedAction -> Action
unselect (SelectedAction commonData actionModel) =
    Action commonData


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


getStats : ActionType -> Stats
getStats actionType =
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
