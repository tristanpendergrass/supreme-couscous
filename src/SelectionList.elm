module SelectionList exposing
    ( SelectionList
    , clearSelection
    , create
    , getLength
    , getSelected
    , map
    , mapSelection
    , push
    , select
    )

import List.Extra
import Maybe.Extra


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
            List.drop (index + 1) list
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


map : (t -> a) -> (( t, d ) -> a) -> a -> SelectionList t d -> List a
map mapEl mapSelectedEl mapNull selectionList =
    let
        mapMaybeEl : Maybe t -> a
        mapMaybeEl maybeEl =
            case maybeEl of
                Just el ->
                    mapEl el

                Nothing ->
                    mapNull
    in
    case selectionList of
        HasSelection _ first ( el, data ) second ->
            List.concat
                [ List.map mapMaybeEl first
                , [ mapSelectedEl ( el, data ) ]
                , List.map mapMaybeEl second
                ]

        NoSelection _ list ->
            List.map mapMaybeEl list


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
