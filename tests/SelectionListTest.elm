module SelectionListTest exposing (..)

import Expect exposing (Expectation)
import Random
import SelectionList exposing (SelectionList)
import Test exposing (Test, describe, test)


zeroGenerator : Random.Generator Int
zeroGenerator =
    Random.constant 0


toZ _ =
    'z'


baseList : SelectionList Char String
baseList =
    SelectionList.create [ 'a', 'b', 'c' ]


emptyList : SelectionList Char String
emptyList =
    SelectionList.create []


selectedList : SelectionList Char String
selectedList =
    baseList
        |> SelectionList.select 1 "data"
        |> Result.withDefault baseList


suite : Test
suite =
    describe "SelectionList"
        [ describe "create"
            [ test "creates a SelectionList" <|
                \_ ->
                    baseList
                        |> SelectionList.toList
                        |> Expect.equal [ 'a', 'b', 'c' ]
            ]
        , describe "select"
            [ test "can select an element in range" <|
                \_ ->
                    baseList
                        |> SelectionList.select 0 "data"
                        |> Result.map SelectionList.getSelected
                        |> Expect.equal (Ok (Just ( 'a', "data" )))
            , test "item order is the same after selecting" <|
                \_ ->
                    baseList
                        |> SelectionList.select 0 "data"
                        |> Result.map SelectionList.toList
                        |> Expect.equal (Ok [ 'a', 'b', 'c' ])
            , test "list is the same after selecting the last element" <|
                \_ ->
                    baseList
                        |> SelectionList.select 2 "data"
                        |> Result.map SelectionList.toList
                        |> Expect.equal (Ok [ 'a', 'b', 'c' ])
            , test "returns an Err if trying to select out of range" <|
                \_ ->
                    baseList
                        |> SelectionList.select 3 "data"
                        |> Result.map SelectionList.getSelected
                        |> Expect.err
            ]
        , describe "mapSelection"
            [ test "maps selection" <|
                \_ ->
                    let
                        toBar ( item, _ ) =
                            ( item, "bar" )
                    in
                    selectedList
                        |> SelectionList.mapSelection toBar
                        |> Result.map SelectionList.getSelected
                        |> Expect.equal (Ok (Just ( 'b', "bar" )))
            ]
        , describe "mapNthMember"
            [ test "returns Err if index is out of range" <|
                \_ ->
                    emptyList
                        |> SelectionList.mapNthMember identity 1
                        |> Expect.err
            , test "maps the nth member when nothing is selected" <|
                \_ ->
                    baseList
                        |> SelectionList.mapNthMember toZ 1
                        |> Expect.equal (Ok (SelectionList.create [ 'a', 'z', 'c' ]))
            , test "maps the selected member" <|
                \_ ->
                    let
                        expectedList =
                            selectedList
                                |> SelectionList.mapSelection (\_ -> ( 'z', "data" ))
                                |> Result.withDefault (SelectionList.create [ 'a', 'b', 'z' ])
                    in
                    selectedList
                        |> SelectionList.mapNthMember toZ 1
                        |> Expect.equal (Ok expectedList)
            ]
        ]
