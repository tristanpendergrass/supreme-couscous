module Main exposing (main)

import Action exposing (ActionType(..))
import Ally
import Engine


allyOne : Ally.Stats
allyOne =
    { avatarUrl = "thief_portrait.png"
    , battleUrl = "thief.png"
    , tombstoneUrl = "thief_tombstone.png"
    , moves =
        [ ThiefAttack
        ]
    , maxEnergy = 100
    }


allyTwo : Ally.Stats
allyTwo =
    { avatarUrl = "knight_portrait.png"
    , battleUrl = "knight.png"
    , tombstoneUrl = "knight_tombstone.png"
    , moves =
        [ KnightAttack
        ]
    , maxEnergy = 150
    }


allyThree : Ally.Stats
allyThree =
    { avatarUrl = "priest_portrait.png"
    , battleUrl = "priest.png"
    , tombstoneUrl = "priest_tombstone.png"
    , moves =
        [ PriestAttack
        ]
    , maxEnergy = 185
    }


enemy : Engine.EngineArgEnemy
enemy =
    { battleUrl = "red_boy.png", moves = [ EnemyAttack ], maxHealth = 80, maxEnergy = 30, damage = 25 }


main : Engine.Instance
main =
    Engine.create
        { title = "A Bun Worth Fighting For"
        , initialParty =
            [ allyOne, allyTwo, allyThree ]
        , initialEnemy = enemy
        }
