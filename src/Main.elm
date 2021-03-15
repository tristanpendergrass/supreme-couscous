module Main exposing (main)

import Engine


main : Engine.Instance
main =
    Engine.create
        { title = "A Bun Worth Fighting For"
        , allyOne = { avatarUrl = "thief_portrait.png", battleUrl = "thief.png" }
        , allyTwo = { avatarUrl = "knight_portrait.png", battleUrl = "knight.png" }
        , allyThree = { avatarUrl = "priest_portrait.png", battleUrl = "priest.png" }
        }
