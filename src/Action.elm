module Action exposing (Action, ActionType(..), stats)

import ActionTimer exposing (ActionTimer)


type alias Action =
    { id : Int
    , timer : ActionTimer
    , actionType : ActionType
    }


type ActionType
    = KnightAttack
    | ThiefAttack
    | PriestAttack
    | EnemyAttack


type alias Stats =
    { avatarUrl : String
    , maxEnergy : Int
    }


stats : Action -> Stats
stats action =
    case action.actionType of
        KnightAttack ->
            { avatarUrl = ""
            , maxEnergy = 100
            }

        ThiefAttack ->
            { avatarUrl = ""
            , maxEnergy = 100
            }

        PriestAttack ->
            { avatarUrl = ""
            , maxEnergy = 100
            }

        EnemyAttack ->
            { avatarUrl = ""
            , maxEnergy = 100
            }
