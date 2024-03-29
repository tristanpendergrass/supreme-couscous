module SelectionList exposing
    ( SelectionList
    , clearSelection
    , create
    , deleteSelected
    , filterUnselected
    , getLength
    , getSelected
    , indexedMap
    , mapSelection
    , mapUnselected
    , push
    , select
    )

import Json.Decode exposing (maybe)
import List.Extra
import Maybe.Extra
import Selection exposing (Selection)


type SelectionList t d
    = HasSelection Int (List (Maybe t)) ( t, d ) (List (Maybe t))
    | NoSelection Int (List (Maybe t))


create : Int -> SelectionList t d
create length =
    NoSelection length (List.repeat length Nothing)


getLength : SelectionList t d -> Int
getLength selectionList =
    case selectionList of
        HasSelection length _ _ _ ->
            length

        NoSelection length _ ->
            length


clearSelection : SelectionList t d -> SelectionList t d
clearSelection list =
    case list of
        NoSelection _ _ ->
            list

        HasSelection length first ( el, _ ) second ->
            NoSelection length (List.concat [ first, [ Just el ], second ])


select : Int -> (t -> d) -> SelectionList t d -> Maybe (SelectionList t d)
select index getData selectionList =
    let
        length =
            getLength selectionList

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
                    Just ( el, getData el )

                _ ->
                    Nothing

        newSecond =
            List.drop (index + 1) list
    in
    if index < List.length list then
        maybeEl
            |> Maybe.map
                (\newSelection ->
                    HasSelection length newFirst newSelection newSecond
                )

    else
        Nothing


mapSelection : (d -> d) -> SelectionList t d -> Maybe (SelectionList t d)
mapSelection fn selectionList =
    case selectionList of
        NoSelection _ _ ->
            Nothing

        HasSelection length first ( el, data ) second ->
            Just <| HasSelection length first ( el, fn data ) second


getSelected : SelectionList t d -> Maybe ( t, d )
getSelected selectionList =
    case selectionList of
        NoSelection _ _ ->
            Nothing

        HasSelection _ _ selection _ ->
            Just selection


indexedMap : (Int -> t -> a) -> (Int -> ( t, d ) -> a) -> (Int -> a) -> SelectionList t d -> List a
indexedMap mapEl mapSelectedEl mapNull selectionList =
    let
        mapMaybeEl : Int -> Maybe t -> a
        mapMaybeEl index maybeEl =
            case maybeEl of
                Just el ->
                    mapEl index el

                Nothing ->
                    mapNull index
    in
    case selectionList of
        HasSelection _ first el second ->
            List.concat
                [ List.indexedMap mapMaybeEl first
                , [ mapSelectedEl (List.length first) el ]
                , List.indexedMap (\index maybeEl -> mapMaybeEl (index + List.length first + 1) maybeEl) second
                ]

        NoSelection _ list ->
            List.indexedMap mapMaybeEl list


{-| Attemtps to find the first Nothing in a list of Maybes and replace it with the given element. Returns Nothing if no suitable
spot was found.
-}
attemptPushToList : a -> List (Maybe a) -> Maybe (List (Maybe a))
attemptPushToList el list =
    case List.Extra.splitWhen Maybe.Extra.isNothing list of
        Just ( beforeNothing, _ :: afterNothing ) ->
            Just (List.concat [ beforeNothing, [ Just el ], afterNothing ])

        _ ->
            Nothing


push : t -> SelectionList t d -> SelectionList t d
push el selectionList =
    case selectionList of
        NoSelection length list ->
            attemptPushToList el list
                |> Maybe.map (NoSelection length)
                |> Maybe.withDefault selectionList

        HasSelection length first selectedEl second ->
            let
                attemptPushToFirst =
                    attemptPushToList el first
                        |> Maybe.map (\newFirst -> HasSelection length newFirst selectedEl second)

                attemptPushToSecond =
                    attemptPushToList el second
                        |> Maybe.map (\newSecond -> HasSelection length first selectedEl newSecond)
            in
            attemptPushToFirst
                |> Maybe.Extra.orElse attemptPushToSecond
                |> Maybe.withDefault selectionList


deleteSelected : SelectionList t d -> SelectionList t d
deleteSelected selectionList =
    case selectionList of
        NoSelection _ _ ->
            selectionList

        HasSelection length first _ second ->
            NoSelection length (List.concat [ first, Nothing :: second ])


mapUnselected : (t -> t) -> SelectionList t d -> SelectionList t d
mapUnselected fn selectionList =
    let
        mapList : List (Maybe t) -> List (Maybe t)
        mapList =
            List.map (Maybe.map fn)
    in
    case selectionList of
        NoSelection length list ->
            NoSelection length (mapList list)

        HasSelection length first maybeEl second ->
            HasSelection length (mapList first) maybeEl (mapList second)


filterUnselected : (t -> Bool) -> SelectionList t d -> SelectionList t d
filterUnselected fn selectionList =
    let
        filterEl : Maybe t -> Maybe t
        filterEl =
            Maybe.andThen
                (\el ->
                    if fn el then
                        Just el

                    else
                        Nothing
                )
    in
    case selectionList of
        NoSelection length list ->
            NoSelection length (List.map filterEl list)

        HasSelection length first maybeEl second ->
            HasSelection length (List.map filterEl first) maybeEl (List.map filterEl second)
