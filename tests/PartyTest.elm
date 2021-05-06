module PartyTest exposing (..)

import Expect exposing (Expectation)
import Party exposing (Party, Selection)
import Random
import Test exposing (Test, describe, test)


type alias Input =
    ( Char, String )


selection : Selection
selection =
    { liveInputs = [] }


selectionWithInput : Selection
selectionWithInput =
    { liveInputs = [ ( 'a', "foo" ) ] }


zeroGenerator : Random.Generator Int
zeroGenerator =
    Random.constant 0


toZ _ =
    'z'


baseList : Party Char
baseList =
    Party.create [ 'a', 'b', 'c' ]


emptyList : Party Char
emptyList =
    Party.create []


selectedList : Party Char
selectedList =
    baseList
        |> Party.select 1 selection
        |> Result.withDefault baseList


suite : Test
suite =
    describe "Party"
        [ describe "create"
            [ test "creates a Party" <|
                \_ ->
                    baseList
                        |> Party.toList
                        |> Expect.equal [ 'a', 'b', 'c' ]
            ]
        , describe "select"
            [ test "can select an element in range" <|
                \_ ->
                    baseList
                        |> Party.select 0 selection
                        |> Result.map Party.getSelected
                        |> Expect.equal (Ok (Just ( 'a', selection )))
            , test "item order is the same after selecting" <|
                \_ ->
                    baseList
                        |> Party.select 0 selection
                        |> Result.map Party.toList
                        |> Expect.equal (Ok [ 'a', 'b', 'c' ])
            , test "list is the same after selecting the last element" <|
                \_ ->
                    baseList
                        |> Party.select 2 selection
                        |> Result.map Party.toList
                        |> Expect.equal (Ok [ 'a', 'b', 'c' ])
            , test "returns an Err if trying to select out of range" <|
                \_ ->
                    baseList
                        |> Party.select 3 selection
                        |> Result.map Party.getSelected
                        |> Expect.err
            ]
        , describe "mapSelection"
            [ test "maps selection" <|
                \_ ->
                    let
                        toBar ( item, _ ) =
                            ( item, selectionWithInput )
                    in
                    selectedList
                        |> Party.mapSelection toBar
                        |> Result.map Party.getSelected
                        |> Expect.equal (Ok (Just ( 'b', selectionWithInput )))
            ]
        , describe "mapNthMember"
            [ test "returns Err if index is out of range" <|
                \_ ->
                    emptyList
                        |> Party.mapNthMember identity 1
                        |> Expect.err
            , test "maps the nth member when nothing is selected" <|
                \_ ->
                    baseList
                        |> Party.mapNthMember toZ 1
                        |> Expect.equal (Ok (Party.create [ 'a', 'z', 'c' ]))
            , test "maps the selected member" <|
                \_ ->
                    let
                        expectedList =
                            selectedList
                                |> Party.mapSelection (\_ -> ( 'z', selection ))
                                |> Result.withDefault (Party.create [ 'a', 'b', 'z' ])
                    in
                    selectedList
                        |> Party.mapNthMember toZ 1
                        |> Expect.equal (Ok expectedList)
            ]
        ]
