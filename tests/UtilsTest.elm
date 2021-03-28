module UtilsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils


suite : Test
suite =
    describe "Utils"
        [ describe "getNextInput"
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
        , describe "isPatternComplete"
            [ test "Returns True for empty pattern" <|
                \_ ->
                    Utils.isPatternComplete [] [ 'a' ]
                        |> Expect.equal True
            , test "Returns True when pattern matches list" <|
                \_ ->
                    Utils.isPatternComplete [ 'a', 'b' ] [ 'a', 'b' ]
                        |> Expect.equal True
            , test "Returns False when pattern doesn't match list" <|
                \_ ->
                    Utils.isPatternComplete [ 'a', 'b', 'c' ] [ 'a', 'b' ]
                        |> Expect.equal False
            , test "Returns True when list has extra inputs than the pattern" <|
                \_ ->
                    Utils.isPatternComplete [ 'a', 'b', 'c' ] [ 'a', 'x', 'b', 'y', 'c', 'z' ]
                        |> Expect.equal True
            ]
        ]
