module ActionTimerTest exposing (..)

import ActionTimer
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ActionTimer"
        [ test "Initializes all the way to the left" <|
            \_ ->
                ActionTimer.create { slideOutTime = 1000, stayTime = 1000, slideInTime = 1000 }
                    |> ActionTimer.getLeft
                    |> Expect.within (Expect.Absolute 0.00001) -100
        , test "Increments correctly to 50%" <|
            \_ ->
                ActionTimer.create { slideOutTime = 1000, stayTime = 1000, slideInTime = 1000 }
                    |> ActionTimer.handleAnimationFrame 500
                    |> ActionTimer.getLeft
                    |> Expect.within (Expect.Absolute 0.00001) -50
        , test "Increments correctly to all the way out" <|
            \_ ->
                ActionTimer.create { slideOutTime = 1000, stayTime = 1000, slideInTime = 1000 }
                    |> ActionTimer.handleAnimationFrame 250
                    |> ActionTimer.handleAnimationFrame 250
                    |> ActionTimer.handleAnimationFrame 250
                    |> ActionTimer.handleAnimationFrame 250
                    |> ActionTimer.getLeft
                    |> Expect.within (Expect.Absolute 0.00001) 0
        , test "Increments correctly to 50% back in" <|
            \_ ->
                ActionTimer.create { slideOutTime = 1000, stayTime = 1000, slideInTime = 1000 }
                    |> ActionTimer.handleAnimationFrame 2000
                    |> ActionTimer.handleAnimationFrame 500
                    |> ActionTimer.getLeft
                    |> Expect.within (Expect.Absolute 0.00001) -50
        , test "Increments correctly to sliding all the way back in" <|
            \_ ->
                ActionTimer.create { slideOutTime = 1000, stayTime = 1000, slideInTime = 1000 }
                    |> ActionTimer.handleAnimationFrame 3000
                    |> ActionTimer.getLeft
                    |> Expect.within (Expect.Absolute 0.00001) -100
        ]
