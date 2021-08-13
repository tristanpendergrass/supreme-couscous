module Action exposing
    ( Action
    , ActionModel(..)
    , ActionType(..)
    , Effect(..)
    , KnightMove(..)
    , create
    , damage
    , getActionType
    , getInputForKnightMove
    , getModel
    , getStats
    , getTimer
    , handleKnightMove
    , isExpired
    , knightMoveStats
    , mapTimer
    )

import ActionTimer exposing (ActionTimer)
import Input exposing (Input)


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
    = KnightModel (List KnightMove)
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


type KnightMove
    = Slash
    | Kick
    | Wait
    | Thrust


knightMoveStats : KnightMove -> { input : Input }
knightMoveStats knightInput =
    case knightInput of
        Slash ->
            { input = Input.Slash }

        Kick ->
            { input = Input.Kick }

        Wait ->
            { input = Input.Wait }

        Thrust ->
            { input = Input.Thrust }


handleKnightMove : KnightMove -> List KnightMove -> List KnightMove
handleKnightMove =
    (::)


getInputForKnightMove : KnightMove -> Input
getInputForKnightMove knightMove =
    case knightMove of
        Slash ->
            Input.Slash

        Kick ->
            Input.Kick

        Wait ->
            Input.Wait

        Thrust ->
            Input.Thrust
