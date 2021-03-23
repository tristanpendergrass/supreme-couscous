module UtilsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils


suite : Test
suite =
    describe "getNextInput"
        [ test "Returns nothing for empty lists" <|
            \_ ->
                Utils.getNextInput [] []
                    |> Expect.equal Nothing
        , test "Returns the correct element for pattern with one element" <|
            \_ ->
                Utils.getNextInput [ 'a' ] []
                    |> Expect.equal (Just 'a')
        , test "Returns Nothing for liveInputs equal to pattern" <|
            \_ ->
                Utils.getNextInput [ 'a' ] [ 'a' ]
                    |> Expect.equal Nothing
        , test "Returns Nothing for liveInputs with extra stuff and equal to pattern" <|
            \_ ->
                Utils.getNextInput [ 'a' ] [ 'y', 'a', 'z' ]
                    |> Expect.equal Nothing
        , test "Returns the correct element for liveInputs with extra stuff" <|
            \_ ->
                Utils.getNextInput [ 'a' ] [ 'y', 'z' ]
                    |> Expect.equal (Just 'a')
        , test "Returns the correct element for pattern that's longer" <|
            \_ ->
                Utils.getNextInput [ 'a', 'b', 'c' ] []
                    |> Expect.equal (Just 'a')
        , test "Returns the correct element for pattern that's longer and partially filled" <|
            \_ ->
                Utils.getNextInput [ 'a', 'b', 'c' ] [ 'a', 'b', 'z' ]
                    |> Expect.equal (Just 'c')
        , test "Returns correct letter in case of repeat" <|
            \_ ->
                Utils.getNextInput [ 'a', 'b', 'c' ] [ 'a', 'b', 'z' ]
                    |> Expect.equal (Just 'c')
        ]
