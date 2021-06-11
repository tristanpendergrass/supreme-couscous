module SelectionListTest exposing (..)

import Expect
import SelectionList exposing (SelectionList)
import Test exposing (Test, describe, test)


toString : SelectionList Int Int -> String
toString selectionList =
    let
        mapEl =
            String.fromInt

        mapSelectedEl ( el, data ) =
            "(" ++ String.fromInt el ++ ", " ++ String.fromInt data ++ ")"

        mapNothing =
            "Nothing"
    in
    selectionList
        |> SelectionList.map mapEl mapSelectedEl mapNothing
        |> String.join ", "


suite : Test
suite =
    describe "SelectionList"
        [ test "Can be created, pushed to, and mapped" <|
            \_ ->
                SelectionList.create 4
                    |> SelectionList.push 2
                    |> SelectionList.push 4
                    |> SelectionList.push 6
                    |> toString
                    |> Expect.equal "2, 4, 6, Nothing"
        , test "Can have items selected" <|
            \_ ->
                SelectionList.create 4
                    |> SelectionList.push 2
                    |> SelectionList.push 4
                    |> SelectionList.push 6
                    |> SelectionList.select 0 1
                    |> Result.map toString
                    |> Result.map (Expect.equal "2, (4, 0), 6, Nothing")
                    |> Result.withDefault (Expect.fail "Result was Err")
        , test "Can have its selection deleted" <|
            \_ ->
                SelectionList.create 4
                    |> SelectionList.push 2
                    |> SelectionList.push 4
                    |> SelectionList.push 6
                    |> SelectionList.select 0 1
                    |> Result.map SelectionList.deleteSelected
                    |> Result.map toString
                    |> Result.map (Expect.equal "2, Nothing, 6, Nothing")
                    |> Result.withDefault (Expect.fail "Result was Err")
        ]
