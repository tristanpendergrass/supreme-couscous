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
    Debug.todo "Implement getStats"
