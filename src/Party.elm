module Party exposing
    ( Party
    , Selection
    , clearSelection
    , create
    , getAt
    , getSelected
    , map
    , mapNthMember
    , mapRandomMember
    , mapSelection
    , mapToList
    , select
    , toList
    )

import List.Extra
import Random


type alias Input =
    ( Char, String )


type alias Selection =
    { liveInputs : List Input
    }


{-| List of values of type a, with one of them optionally selected. If selected, data of type t is attached to the selected item.
-}
type Party a
    = SelectionList (List a) (Maybe ( a, Selection )) (List a)


create : List a -> Party a
create initial =
    SelectionList initial Nothing []


toList : Party a -> List a
toList (SelectionList first maybeEl second) =
    case maybeEl of
        Nothing ->
            List.concat [ first, second ]

        Just ( el, _ ) ->
            List.concat [ first, [ el ], second ]


getSelected : Party a -> Maybe ( a, Selection )
getSelected (SelectionList _ el _) =
    el


clearSelection : Party a -> Party a
clearSelection =
    toList >> create


getAt : Int -> Party a -> Maybe a
getAt position =
    toList >> List.Extra.getAt position


select : Int -> Selection -> Party a -> Result String (Party a)
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


mapSelection : (( a, Selection ) -> ( a, Selection )) -> Party a -> Result String (Party a)
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


mapToList : (Bool -> Int -> a -> b) -> Party a -> List b
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


map : (a -> b) -> Party a -> Party b
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



-- getRandomIndex : a -> SelectionList a t -> Random.Generator (Maybe Int)
-- getRandomIndex selectionList =
--     let
--         lower =
--             0
--         higher =
--             selectionList
--                 |> toList
--                 |> List.length
--                 |> (+) 1
--     in
--     Random.int lower higher


mapNthMember : (a -> a) -> Int -> Party a -> Result String (Party a)
mapNthMember mapFn index (SelectionList first maybeEl second) =
    case maybeEl of
        Nothing ->
            if index < 0 then
                Err "Index not in range"

            else if index < List.length first then
                Ok (SelectionList (List.Extra.updateAt index mapFn first) maybeEl second)

            else if index < List.length first + List.length second then
                Ok (SelectionList first maybeEl (List.Extra.updateAt (List.length first + index) mapFn second))

            else
                Err "Index not in range"

        Just ( el, data ) ->
            if index < 0 then
                Err "Index not in range"

            else if index < List.length first then
                Ok (SelectionList (List.Extra.updateAt index mapFn first) maybeEl second)

            else if index == List.length first then
                Ok (SelectionList first (Just ( mapFn el, data )) second)

            else if index < List.length first + List.length second then
                Ok (SelectionList first maybeEl (List.Extra.updateAt (List.length first + index) mapFn second))

            else
                Err "Index not in range"


mapRandomMember : (a -> a) -> Party a -> Random.Generator (Party a)
mapRandomMember mapFn selectionList =
    case selectionList of
        SelectionList [] Nothing [] ->
            Random.constant selectionList

        SelectionList _ _ _ ->
            let
                upperBound =
                    selectionList
                        |> toList
                        |> List.length

                indexGenerator : Random.Generator Int
                indexGenerator =
                    Random.int 0 upperBound
            in
            indexGenerator
                |> Random.map (\index -> mapNthMember mapFn index selectionList)
                |> Random.map (Result.withDefault selectionList)
