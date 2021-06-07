module SelectionList exposing
    ( SelectionList
    , clearSelection
    , create
    , getSelected
    , mapSelection
    , select
    )

import List.Extra


type SelectionList t d
    = HasSelection Int (List (Maybe t)) ( t, d ) (List (Maybe t))
    | NoSelection Int (List (Maybe t))


create : Int -> SelectionList t d
create length =
    NoSelection length (List.repeat length Nothing)


clearSelection : SelectionList t d -> SelectionList t d
clearSelection list =
    case list of
        NoSelection _ _ ->
            list

        HasSelection length first ( el, _ ) second ->
            NoSelection length (List.concat [ first, [ Just el ], second ])


getLength : SelectionList t d -> Int
getLength selectionList =
    case selectionList of
        NoSelection length _ ->
            length

        HasSelection length _ _ _ ->
            length


select : d -> Int -> SelectionList t d -> Result String (SelectionList t d)
select initialData index selectionList =
    let
        list =
            case selectionList of
                NoSelection _ list_ ->
                    list_

                HasSelection _ first ( el, _ ) second ->
                    List.concat [ first, [ Just el ], second ]

        newFirst =
            List.take index list

        maybeEl : Maybe ( t, d )
        maybeEl =
            case List.Extra.getAt index list of
                Just (Just el) ->
                    Just ( el, initialData )

                _ ->
                    Nothing

        newSecond =
            List.drop index list
    in
    if index < List.length list then
        case maybeEl of
            Nothing ->
                Err "Trying to select Nothing"

            Just newSelection ->
                Ok (HasSelection (getLength selectionList) newFirst newSelection newSecond)

    else
        Err "Index is out of bounds"


mapSelection : (t -> d -> d) -> SelectionList t d -> Result String (SelectionList t d)
mapSelection fn selectionList =
    case selectionList of
        NoSelection _ _ ->
            Err "Nothing is selected"

        HasSelection length first ( el, data ) second ->
            Ok <| HasSelection length first ( el, fn el data ) second


getSelected : SelectionList t d -> Result String ( t, d )
getSelected selectionList =
    case selectionList of
        NoSelection _ _ ->
            Err "Nothing is selected"

        HasSelection _ _ selection _ ->
            Ok selection
