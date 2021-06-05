module ActionList exposing (ActionList, create, getSelected, toListWithSelectionStatus)

import Ally exposing (Ally, Move)


type ActionList
    = ActionList (List (Maybe Move)) (Maybe ( Move, Selection )) (List (Maybe Move)) (List Move)


type alias Input =
    ( Char, String )


type alias Selection =
    { liveInputs : List Input
    , move : Move
    }


create : ActionList
create =
    ActionList [] Nothing [] []


toList : ActionList -> List (Maybe Move)
toList (ActionList first maybeEl second reserve) =
    case maybeEl of
        Nothing ->
            List.concat [ first, second ]

        Just ( el, _ ) ->
            List.concat [ first, [ Just el ], second ]


toListWithSelectionStatus : ActionList -> List (Maybe ( Move, Bool ))
toListWithSelectionStatus (ActionList first maybeEl second _) =
    let
        notSelected : Maybe Move -> Maybe ( Move, Bool )
        notSelected =
            Maybe.map
                (\move ->
                    ( move, False )
                )
    in
    case maybeEl of
        Nothing ->
            List.concat [ first, second ]
                |> List.map notSelected

        Just ( move, _ ) ->
            List.concat [ List.map notSelected first, [ Just ( move, True ) ], List.map notSelected second ]


getSelected : ActionList -> Maybe ( Move, Selection )
getSelected (ActionList _ el _ _) =
    el
