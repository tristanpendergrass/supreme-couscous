module Input exposing (Input(..), getStats, matchStringToInput)


type Input
    = SelectOne
    | SelectTwo
    | SelectThree
    | SelectFour
    | SelectFive
    | Cancel
    | Finish
      -- Knight moves
    | Slash
    | Kick
    | Wait
    | Thrust


matchStringToInput : String -> Maybe Input
matchStringToInput string =
    case string of
        "1" ->
            Just SelectOne

        "2" ->
            Just SelectTwo

        "3" ->
            Just SelectThree

        "4" ->
            Just SelectFour

        "5" ->
            Just SelectFive

        "Escape" ->
            Just Cancel

        "q" ->
            Just Cancel

        "Enter" ->
            Just Finish

        "s" ->
            Just Slash

        "k" ->
            Just Kick

        "w" ->
            Just Wait

        "t" ->
            Just Thrust

        _ ->
            Nothing


type alias InputStats =
    { shortcutLabel : String, label : String }


getStats : Input -> InputStats
getStats input =
    case input of
        SelectOne ->
            { shortcutLabel = "1", label = "1" }

        SelectTwo ->
            { shortcutLabel = "2", label = "2" }

        SelectThree ->
            { shortcutLabel = "3", label = "3" }

        SelectFour ->
            { shortcutLabel = "4", label = "4" }

        SelectFive ->
            { shortcutLabel = "5", label = "5" }

        Slash ->
            { shortcutLabel = "s", label = "Slash" }

        Kick ->
            { shortcutLabel = "k", label = "Kick" }

        Wait ->
            { shortcutLabel = "w", label = "Wait" }

        Thrust ->
            { shortcutLabel = "t", label = "Thrust" }

        Cancel ->
            { shortcutLabel = "q", label = "Cancel" }

        Finish ->
            { shortcutLabel = "Enter", label = "Finish" }
