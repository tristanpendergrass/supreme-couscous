module SelectionListTest exposing (..)

import Expect
import SelectionList
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SelectionList"
        [ describe "create"
            [ test "creates a SelectionList" <|
                \_ ->
                    SelectionList.create [ 'a', 'b', 'c' ]
                        |> SelectionList.toList
                        |> Expect.equal [ 'a', 'b', 'c' ]
            ]
        , describe "select"
            [ test "can select an element in range" <|
                \_ ->
                    SelectionList.create [ 'a', 'b', 'c' ]
                        |> SelectionList.select 0 "data"
                        |> Result.map SelectionList.getSelected
                        |> Expect.equal (Ok (Just ( 'a', "data" )))
            , test "returns an Err if trying to select out of range" <|
                \_ ->
                    SelectionList.create [ 'a', 'b', 'c' ]
                        |> SelectionList.select 3 "data"
                        |> Result.map SelectionList.getSelected
                        |> Expect.err
            ]
        , describe "mapSelectionData"
            [ test "maps selection data" <|
                \_ ->
                    let
                        toBar _ =
                            "bar"
                    in
                    SelectionList.create [ 'a', 'b', 'c' ]
                        |> SelectionList.select 0 "foo"
                        |> Result.andThen (SelectionList.mapSelectionData toBar)
                        |> Result.map SelectionList.getSelected
                        |> Expect.equal (Ok (Just ( 'a', "bar" )))
            ]
        ]
