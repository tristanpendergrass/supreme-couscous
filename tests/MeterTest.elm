module MeterTest exposing (..)

import Expect
import Meter exposing (Meter)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Meter"
        [ describe "handleAnimationFrameRegen"
            [ test "correctly increments the meter" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.drain
                        |> Meter.handleAnimationFrameRegen 10.0
                        |> Meter.getCurrent
                        |> Expect.within (Expect.Absolute 0.000001) 0.1
            ]
        , describe "handleAnimationFrameDebounce"
            [ test "full meter" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.getDebouncedCurrent
                        |> Expect.within (Expect.Absolute 0.000001) 100.0
            , test "after subtracting current" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.subtract 10
                        |> Meter.getDebouncedCurrent
                        |> Expect.within (Expect.Absolute 0.000001) 100.0
            , test "after some time has passed it is still waiting" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.subtract 10
                        |> Meter.handleAnimationFrameDebounce 750
                        |> Meter.getDebouncedCurrent
                        |> Expect.within (Expect.Absolute 0.000001) 100.0
            , test "after even more time has passed it is ticking down" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.subtract 10
                        |> Meter.handleAnimationFrameDebounce (1000 + 250)
                        |> Meter.getDebouncedCurrent
                        |> Expect.within (Expect.Absolute 0.000001) 95.0
            , test "ticks 80% of the way down" <|
                \_ ->
                    Meter.create 100.0
                        |> Meter.subtract 10
                        |> Meter.handleAnimationFrameDebounce 1400
                        |> Meter.getDebouncedCurrent
                        |> Expect.within (Expect.Absolute 0.000001) 92
            ]
        ]
