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
    = SelectionList (List (Maybe t)) (Maybe ( t, d )) (List (Maybe t))


create : SelectionList t d
create =
    SelectionList [] Nothing []


clearSelection : SelectionList t d -> SelectionList t d
clearSelection =
    toList >> fromList


toList : SelectionList t d -> List (Maybe t)
toList (SelectionList first maybeEl second) =
    case maybeEl of
        Nothing ->
            List.concat
                [ first, second ]

        Just ( el, _ ) ->
            List.concat
                [ first, [ Just el ], second ]


fromList : List (Maybe t) -> SelectionList t d
fromList list =
    SelectionList list Nothing []


select : d -> Int -> SelectionList t d -> Result String (SelectionList t d)
select initialData index selectionList =
    let
        list : List (Maybe t)
        list =
            toList selectionList

        first =
            List.take index list

        maybeEl : Maybe ( t, d )
        maybeEl =
            case List.Extra.getAt index list of
                Just (Just el) ->
                    Just ( el, initialData )

                _ ->
                    Nothing

        second =
            List.drop index list
    in
    if index < List.length list then
        Ok (SelectionList first maybeEl second)

    else
        Err "Index is out of bounds"


mapSelection : (d -> d) -> SelectionList t d -> Result String (SelectionList t d)
mapSelection fn (SelectionList first maybeEl second) =
    case maybeEl of
        Nothing ->
            Err "Index is out of bounds"

        Just ( el, data ) ->
            Ok <| SelectionList first (Just ( el, fn data )) second


getSelected : SelectionList t d -> Result String ( t, d )
getSelected (SelectionList _ maybeEl _) =
    case maybeEl of
        Nothing ->
            Err "Index is out of bounds"

        Just ( el, data ) ->
            Ok <| ( el, data )
