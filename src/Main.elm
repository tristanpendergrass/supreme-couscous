module Main exposing (main)

import Engine


allyOne : Engine.EngineArgAlly
allyOne =
    { avatarUrl = "thief_portrait.png"
    , battleUrl = "thief.png"
    , move =
        { damage = 5
        , prompt = "A quick jab, followed by a slash..."
        , inputs =
            [ ( 'j', "Jab" )
            , ( 's', "Slash" )
            ]
        }
    }


allyTwo : Engine.EngineArgAlly
allyTwo =
    { avatarUrl = "knight_portrait.png"
    , battleUrl = "knight.png"
    , move =
        { damage = 10
        , prompt = "Analyze the opponent's weak spots and then strike twice..."
        , inputs =
            [ ( 'a', "Analyze" )
            , ( 's', "Strike" )
            , ( 's', "Strike" )
            ]
        }
    }


allyThree : Engine.EngineArgAlly
allyThree =
    { avatarUrl = "priest_portrait.png"
    , battleUrl = "priest.png"
    , move =
        { damage = 2
        , prompt = "Focus, determination, and then yell 'hoody hoo!'..."
        , inputs =
            [ ( 'f', "Focus" )
            , ( 'd', "Determination" )
            , ( 'h', "Hoody Hoo!" )
            ]
        }
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
