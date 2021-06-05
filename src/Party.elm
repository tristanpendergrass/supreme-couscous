module Party exposing
    ( AllySpot(..)
    , Party
    ,  create
       -- , damageRandomMember

    , handleAnimationFrame
    , isEveryoneDead
    , map
    )

import Ally exposing (Ally)
import List.Extra
import Meter
import Random


type Party
    = Party (List AllySpot)


type AllySpot
    = DeadAlly Ally.Stats
    | AliveAlly Ally


type alias Input =
    ( Char, String )


create : List Ally -> Party
create =
    List.map AliveAlly >> Party



-- damageRandomMember : Float -> Party -> Random.Generator Party
-- damageRandomMember damageAmount (Party party) =
--     let
--         damageAlly : AllySpot -> AllySpot
--         damageAlly allySpot =
--             case allySpot of
--                 DeadAlly _ ->
--                     allySpot
--                 AliveAlly ally ->
--                     let
--                         newAlly =
--                             ally
--                                 |> Ally.removeHealth damageAmount
--                                 |> Ally.addShake
--                     in
--                     if Meter.isEmpty newAlly.health then
--                         DeadAlly ally.stats
--                     else
--                         AliveAlly newAlly
--     in
--     case indexesOfAliveAllies party of
--         first :: rest ->
--             Random.uniform first rest
--                 |> Random.map
--                     (\randomIndex ->
--                         List.Extra.updateAt randomIndex damageAlly party
--                             |> Party
--                     )
--         [] ->
--             Random.constant (Party party)


handleAnimationFrame : Float -> Party -> Party
handleAnimationFrame delta (Party party) =
    party
        |> List.map
            (\allySpot ->
                case allySpot of
                    DeadAlly stats ->
                        DeadAlly stats

                    AliveAlly ally ->
                        AliveAlly (Ally.handleAnimationFrame delta ally)
            )
        |> Party


isEveryoneDead : Party -> Bool
isEveryoneDead (Party party) =
    party
        |> List.all isAlive



-- Internal


isAlive : AllySpot -> Bool
isAlive allySpot =
    case allySpot of
        AliveAlly _ ->
            True

        DeadAlly _ ->
            False


indexesOfAliveAllies : List AllySpot -> List Int
indexesOfAliveAllies allySpots =
    let
        addIndexIfAlive : Int -> AllySpot -> List Int -> List Int
        addIndexIfAlive index allySpot accum =
            if isAlive allySpot then
                index :: accum

            else
                accum
    in
    allySpots
        |> List.Extra.indexedFoldl addIndexIfAlive []


map : (AllySpot -> t) -> Party -> List t
map fn (Party spots) =
    List.map fn spots
