module Input exposing (Input(..))


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


type alias Shortcut =
    -- (Keycode, Label)
    ( String, String )


getShortcut : Input -> Shortcut
getShortcut input =
    case input of
        SelectOne ->
            ( "1", "1" )

        SelectTwo ->
            ( "2", "2" )

        SelectThree ->
            ( "3", "3" )

        SelectFour ->
            ( "4", "4" )

        SelectFive ->
            ( "5", "5" )

        Cancel ->
            ( "", "" )
