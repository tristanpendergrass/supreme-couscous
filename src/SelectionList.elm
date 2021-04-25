module SelectionList exposing
    ( SelectionList
    , clearSelection
    , create
    , getAt
    , getSelected
    , map
    , mapSelection
    , mapToList
    , select
    , toList
    )

import List.Extra


{-| List of values of type a, with one of them optionally selected. If selected, data of type t is attached to the selected item.
-}
type SelectionList a t
    = SelectionList (List a) (Maybe ( a, t )) (List a)


create : List a -> SelectionList a t
create initial =
    SelectionList initial Nothing []


toList : SelectionList a t -> List a
toList (SelectionList first maybeEl second) =
    case maybeEl of
        Nothing ->
            List.concat [ first, second ]

        Just ( el, _ ) ->
            List.concat [ first, [ el ], second ]


getSelected : SelectionList a t -> Maybe ( a, t )
getSelected (SelectionList _ el _) =
    el


clearSelection : SelectionList a t -> SelectionList a t
clearSelection =
    toList >> create


getAt : Int -> SelectionList a t -> Maybe a
getAt position =
    toList >> List.Extra.getAt position


select : Int -> t -> SelectionList a t -> Result String (SelectionList a t)
select position data oldList =
    let
        list =
            toList oldList
    in
    case List.Extra.getAt position list of
        -- equivalent to myArr[3]
        Nothing ->
            -- out of range
            Result.Err <| "Can't select position " ++ String.fromInt position

        Just selected ->
            Result.Ok (SelectionList (List.take position list) (Just ( selected, data )) (List.drop (position + 1) list))


mapSelection : (( a, t ) -> ( a, t )) -> SelectionList a t -> Result String (SelectionList a t)
mapSelection mapFn (SelectionList first maybeEl second) =
    case maybeEl of
        Nothing ->
            Err "Can't modify data with no selection"

        Just ( el, data ) ->
            let
                newEl =
                    Just (mapFn ( el, data ))
            in
            Ok <| SelectionList first newEl second


mapToList : (Bool -> Int -> a -> b) -> SelectionList a t -> List b
mapToList mapFn (SelectionList first maybeEl second) =
    let
        selectionList =
            SelectionList first maybeEl second
    in
    case maybeEl of
        Nothing ->
            List.indexedMap (mapFn False) (toList selectionList)

        Just _ ->
            let
                selectedIndex =
                    List.length first
            in
            List.indexedMap
                (\index item ->
                    if index == selectedIndex then
                        mapFn True index item

                    else
                        mapFn False index item
                )
                (toList selectionList)


map : (a -> b) -> SelectionList a t -> SelectionList b t
map mapFn (SelectionList first maybeEl second) =
    let
        newFirst =
            List.map mapFn first

        newSecond =
            List.map mapFn second

        newEl =
            maybeEl
                |> Maybe.andThen (\( el, data ) -> Just ( mapFn el, data ))
    in
    SelectionList newFirst newEl newSecond
