module Main exposing (main)

import Ally
import Engine


allyOne : Ally.Stats
allyOne =
    { avatarUrl = "thief_portrait.png"
    , battleUrl = "thief.png"
    , tombstoneUrl = "thief_tombstone.png"
    , move =
        { onSuccess = [ Ally.damage 10 ]
        , prompt = "A quick jab, followed by a slash..."
        , inputs =
            [ ( 'j', "Jab" )
            , ( 's', "Slash" )
            ]
        }
    , maxHealth = 100
    , maxEnergy = 75
    }


allyTwo : Ally.Stats
allyTwo =
    { avatarUrl = "knight_portrait.png"
    , battleUrl = "knight.png"
    , tombstoneUrl = "knight_tombstone.png"
    , move =
        { onSuccess = [ Ally.damage 5 ]
        , prompt = "Analyze the opponent's weak spots and then strike twice..."
        , inputs =
            [ ( 'a', "Analyze" )
            , ( 's', "Strike" )
            , ( 's', "Strike" )
            ]
        }
    , maxHealth = 100
    , maxEnergy = 100
    }


allyThree : Ally.Stats
allyThree =
    { avatarUrl = "priest_portrait.png"
    , battleUrl = "priest.png"
    , tombstoneUrl = "priest_tombstone.png"
    , move =
        { onSuccess = [ Ally.damage 20 ]
        , prompt = "Focus, determination, and then yell 'hoody hoo!'..."
        , inputs =
            [ ( 'f', "Focus" )
            , ( 'd', "Determination" )
            , ( 'h', "Hoody Hoo!" )
            ]
        }
    , maxHealth = 100
    , maxEnergy = 175
    }


enemy : Engine.EngineArgEnemy
enemy =
    { battleUrl = "red_boy.png", maxHealth = 100, maxEnergy = 50, damage = 100 }


main : Engine.Instance
main =
    Engine.create
        { title = "A Bun Worth Fighting For"
        , initialParty =
            [ allyOne, allyTwo, allyThree ]
        , initialEnemy = enemy
        }
