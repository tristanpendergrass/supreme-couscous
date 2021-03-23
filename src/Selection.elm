module Selection exposing (Position, Selection)


type alias Selection =
    { position : Position
    , inputs : List ( Char, String )
    }


type Position
    = First
    | Second
    | Third
