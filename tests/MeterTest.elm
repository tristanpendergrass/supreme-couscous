module MeterTest exposing (..)

import Expect
import Meter exposing (Meter)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Meter"
        [ describe "handleAnimationFrame"
            [ test "correctly increments the meter" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.drain
                        |> Meter.handleAnimationFrame 10.0
                        |> Meter.getCurrent
                        |> Expect.equal 10.0
            ]
        ]
