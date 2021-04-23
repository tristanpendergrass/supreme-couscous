module Main exposing (main)

import Engine


allyOne : Engine.EngineArgAlly
allyOne =
    { avatarUrl = "thief_portrait.png"
    , battleUrl = "thief.png"
    , move =
        { onSuccess = [ Engine.damage 10 ]
        , prompt = "A quick jab, followed by a slash..."
        , inputs =
            [ ( 'j', "Jab" )
            , ( 's', "Slash" )
            ]
        }
    , maxHealth = 100
    , maxEnergy = 75
    }


allyTwo : Engine.EngineArgAlly
allyTwo =
    { avatarUrl = "knight_portrait.png"
    , battleUrl = "knight.png"
    , move =
        { onSuccess = [ Engine.damage 5 ]
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


allyThree : Engine.EngineArgAlly
allyThree =
    { avatarUrl = "priest_portrait.png"
    , battleUrl = "priest.png"
    , move =
        { onSuccess = [ Engine.damage 20 ]
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
    { battleUrl = "red_boy.png", maxHealth = 100 }


main : Engine.Instance
main =
    Engine.create
        { title = "A Bun Worth Fighting For"
        , initialParty =
            [ allyOne, allyTwo, allyThree ]
        , initialEnemy = enemy
        }
