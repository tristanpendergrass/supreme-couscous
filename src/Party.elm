module Party exposing
    ( AllySpot(..)
    , Party
    , Selection
    , clearSelection
    , create
    , getLiveAllyAt
    , getSelected
    , getSelectedAllyIfComplete
    , handleAnimationFrame
    , mapAliveAllies
    , mapNthMember
    , mapRandomMember
    , mapSelection
    , select
    , toList
    ,  toListWithSelectionStatus
       -- , mapToList

    )

import Ally exposing (Ally)
import List.Extra
import Meter exposing (Meter)
import Random
import Utils


type AllySpot
    = DeadAlly Ally.Stats
    | AliveAlly Ally


type alias Input =
    ( Char, String )


type alias Selection =
    { liveInputs : List Input
    }


{-| List of values of type a, with one of them optionally selected. If selected, data of type t is attached to the selected item.
-}
type Party
    = Party (List AllySpot) (Maybe ( Ally, Selection )) (List AllySpot)


create : List Ally -> Party
create allies =
    Party (List.map AliveAlly allies) Nothing []


createFromAllySpot : List AllySpot -> Party
createFromAllySpot allies =
    Party allies Nothing []


toList : Party -> List AllySpot
toList (Party first maybeEl second) =
    case maybeEl of
        Nothing ->
            List.concat [ first, second ]

        Just ( el, _ ) ->
            List.concat [ first, [ AliveAlly el ], second ]


toListWithSelectionStatus : Party -> List ( AllySpot, Bool )
toListWithSelectionStatus (Party first maybeEl second) =
    let
        notSelected : AllySpot -> ( AllySpot, Bool )
        notSelected allySpot =
            ( allySpot, False )
    in
    case maybeEl of
        Nothing ->
            List.concat [ first, second ]
                |> List.map notSelected

        Just ( el, _ ) ->
            List.concat [ List.map notSelected first, [ ( AliveAlly el, True ) ], List.map notSelected second ]


getSelected : Party -> Maybe ( Ally, Selection )
getSelected (Party _ el _) =
    el


clearSelection : Party -> Party
clearSelection =
    toList >> createFromAllySpot


mapLiveAlly : AllySpot -> Maybe Ally
mapLiveAlly allySpot =
    case allySpot of
        AliveAlly ally ->
            Just ally

        DeadAlly _ ->
            Nothing


mapIfAlive : (Ally -> Ally) -> AllySpot -> AllySpot
mapIfAlive fn allySpot =
    case allySpot of
        AliveAlly ally ->
            AliveAlly (fn ally)

        DeadAlly _ ->
            allySpot


getLiveAllyAt : Int -> Party -> Maybe Ally
getLiveAllyAt position =
    toList >> List.Extra.getAt position >> Maybe.andThen mapLiveAlly


select : Int -> Selection -> Party -> Result String Party
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

        Just (DeadAlly _) ->
            Result.Err <| "Can't select dead ally"

        Just (AliveAlly selectedAlly) ->
            Result.Ok (Party (List.take position list) (Just ( selectedAlly, data )) (List.drop (position + 1) list))


mapSelection : (( Ally, Selection ) -> ( Ally, Selection )) -> Party -> Result String Party
mapSelection mapFn (Party first maybeEl second) =
    case maybeEl of
        Nothing ->
            Err "Can't modify data with no selection"

        Just ( el, data ) ->
            let
                newEl =
                    Just (mapFn ( el, data ))
            in
            Ok <| Party first newEl second



-- mapToList : (Bool -> Bool -> Ally -> a) -> Party -> List a
-- mapToList mapFn (SelectionList first maybeEl second) =
--     let
--         selectionList =
--             SelectionList first maybeEl second
--     in
--     case maybeEl of
--         Nothing ->
--             List.map (mapFn False) (toList selectionList)
--         Just _ ->
--             let
--                 selectedIndex =
--                     List.length first
--             in
--             List.map
--                 (\item ->
--                     if index == selectedIndex then
--                         mapFn True index item
--                     else
--                         mapFn False index item
--                 )
--                 (toList selectionList)


mapAliveAllies : (Ally -> Ally) -> Party -> Party
mapAliveAllies fn (Party first maybeEl second) =
    let
        newFirst =
            List.map (mapIfAlive fn) first

        newSecond =
            List.map (mapIfAlive fn) second

        newEl =
            maybeEl
                |> Maybe.andThen
                    (\( el, data ) -> Just ( fn el, data ))
    in
    Party newFirst newEl newSecond



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


mapNthMember : (Ally -> Ally) -> Int -> Party -> Result String Party
mapNthMember fn index (Party first maybeEl second) =
    let
        mapAllySpot =
            mapIfAlive fn
    in
    case maybeEl of
        Nothing ->
            if index < 0 then
                Err "Index not in range"

            else if index < List.length first then
                Ok (Party (List.Extra.updateAt index mapAllySpot first) maybeEl second)

            else if index < List.length first + List.length second then
                Ok (Party first maybeEl (List.Extra.updateAt (List.length first + index) mapAllySpot second))

            else
                Err "Index not in range"

        Just ( el, data ) ->
            if index < 0 then
                Err "Index not in range"

            else if index < List.length first then
                Ok (Party (List.Extra.updateAt index mapAllySpot first) maybeEl second)

            else if index == List.length first then
                Ok (Party first (Just ( fn el, data )) second)

            else if index < List.length first + List.length second then
                Ok (Party first maybeEl (List.Extra.updateAt (List.length first + index) mapAllySpot second))

            else
                Err "Index not in range"


headAndTail : Party -> Maybe ( AllySpot, Party )
headAndTail party =
    case party of
        Party (first :: rest) maybeEl second ->
            Just ( first, Party rest maybeEl second )

        Party [] (Just ( ally, _ )) second ->
            Just ( AliveAlly ally, Party [] Nothing second )

        Party [] Nothing (second :: rest) ->
            Just ( second, Party [] Nothing rest )

        Party [] Nothing [] ->
            Nothing


isAlive : AllySpot -> Bool
isAlive allySpot =
    case allySpot of
        AliveAlly _ ->
            True

        DeadAlly _ ->
            False


{-| Random generator that applies a mapping function to a random alive party member.
Approach:

  - Iterate over party creating list of indexes with a living party member
  - If list is empty then return original list (possible to make this a special return type if need arises)
  - If list has at least one member, use Random.uniform

-}
indexesOfAliveAllies : Party -> List Int
indexesOfAliveAllies =
    let
        addIndexIfAlive : Int -> AllySpot -> List Int -> List Int
        addIndexIfAlive index allySpot accum =
            if isAlive allySpot then
                index :: accum

            else
                accum
    in
    toList >> List.Extra.indexedFoldl addIndexIfAlive []


mapRandomMember : (Ally -> Ally) -> Party -> Random.Generator Party
mapRandomMember mapFn party =
    case indexesOfAliveAllies party of
        first :: rest ->
            Random.uniform first rest
                |> Random.map
                    (\randomIndex ->
                        mapNthMember mapFn randomIndex party
                            |> Result.withDefault party
                    )

        [] ->
            Random.constant party


handleAnimationFrame : Float -> Party -> Party
handleAnimationFrame delta =
    mapAliveAllies (Ally.handleAnimationFrame delta)


getSelectedAllyIfComplete : Party -> Maybe Ally
getSelectedAllyIfComplete party =
    getSelected party
        |> Maybe.andThen
            (\( selectedAlly, { liveInputs } ) ->
                if Utils.isPatternComplete selectedAlly.stats.move.inputs (List.reverse liveInputs) then
                    Just selectedAlly

                else
                    Nothing
            )
