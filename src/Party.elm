module Party exposing
    ( AllySpot(..)
    , Party
    , Selection
    , clearSelection
    , create
    , damageRandomMember
    , getSelected
    , getSelectedAllyIfComplete
    , handleAnimationFrame
    , isEveryoneDead
    , mapSelection
    , selectPosition
    , toListWithSelectionStatus
    )

import Ally exposing (Ally)
import List.Extra
import Meter
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


mapNthMember : (AllySpot -> AllySpot) -> Int -> Party -> Result String Party
mapNthMember fn index (Party first maybeEl second) =
    case maybeEl of
        Nothing ->
            if index < 0 then
                Err "Index not in range"

            else if index < List.length first then
                Ok (Party (List.Extra.updateAt index fn first) maybeEl second)

            else if index < List.length first + List.length second then
                Ok (Party first maybeEl (List.Extra.updateAt (List.length first + index) fn second))

            else
                Err "Index not in range"

        Just ( el, data ) ->
            if index < 0 then
                Err "Index not in range"

            else if index < List.length first then
                Ok (Party (List.Extra.updateAt index fn first) maybeEl second)

            else if index == List.length first then
                let
                    newAllySpot =
                        fn (AliveAlly el)
                in
                case newAllySpot of
                    AliveAlly newAlly ->
                        Ok (Party first (Just ( newAlly, data )) second)

                    DeadAlly stats ->
                        Ok (Party first Nothing (DeadAlly stats :: second))

            else if index < List.length first + List.length second then
                Ok (Party first maybeEl (List.Extra.updateAt (List.length first + index) fn second))

            else
                Err "Index not in range"


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


damageRandomMember : Float -> Party -> Random.Generator Party
damageRandomMember damageAmount party =
    let
        damageAlly : AllySpot -> AllySpot
        damageAlly allySpot =
            case allySpot of
                DeadAlly _ ->
                    allySpot

                AliveAlly ally ->
                    let
                        newAlly =
                            ally
                                |> Ally.removeHealth damageAmount
                                |> Ally.addShake
                    in
                    if Meter.isEmpty newAlly.health then
                        DeadAlly ally.stats

                    else
                        AliveAlly newAlly
    in
    case indexesOfAliveAllies party of
        first :: rest ->
            Random.uniform first rest
                |> Random.map
                    (\randomIndex ->
                        mapNthMember damageAlly randomIndex party
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


selectPosition : Int -> Party -> Maybe Party
selectPosition index party =
    let
        allyHasEnergy =
            getLiveAllyAt index party
                |> Maybe.map (.energy >> Meter.isFull)
                |> Maybe.withDefault False
    in
    if allyHasEnergy then
        let
            list =
                toList party

            initialData : Selection
            initialData =
                { liveInputs = [] }
        in
        List.Extra.getAt index list
            |> Maybe.andThen
                (\allySpot ->
                    case allySpot of
                        DeadAlly _ ->
                            Nothing

                        AliveAlly selectedAlly ->
                            Just (Party (List.take index list) (Just ( selectedAlly, initialData )) (List.drop (index + 1) list))
                )

    else
        Nothing


isEveryoneDead : Party -> Bool
isEveryoneDead =
    toList >> List.all (isAlive >> not)
