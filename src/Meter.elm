module Meter exposing
    ( Color(..)
    , Meter
    , add
    , create
    , createEmpty
    , drain
    , getCurrent
    , getDebouncedCurrent
    , handleAnimationFrameDebounce
    , handleAnimationFrameRegen
    , isEmpty
    , isFull
    , renderHorizontal
    , renderVertical
    , setColor
    , setDisplaySize
    , subtract
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class, default, style)


appendString : String -> String -> String
appendString second first =
    first ++ second


totalWaitTime : Float
totalWaitTime =
    500



-- How many percent per millisecond should be covered by debounce ticking


tickSpeed : Float
tickSpeed =
    0.5


type Color
    = Red
    | Blue
    | Purple


getBgStyle : Color -> String
getBgStyle color =
    case color of
        Red ->
            "bg-red-500"

        Blue ->
            "bg-blue-700"

        Purple ->
            "bg-purple-500"


type DebounceTimer
    = AtRest
    | Waiting Float
    | Ticking


type alias Meter =
    { max : Float
    , current : Float
    , color : Color
    , displaySize : Int
    , debouncedCurrent : Float
    , debounceTimer : DebounceTimer
    }


create : Float -> Meter
create max =
    { max = max
    , current = max
    , color = Red
    , displaySize = 100
    , debouncedCurrent = max
    , debounceTimer = AtRest
    }


createEmpty : Float -> Meter
createEmpty max =
    { max = max
    , current = 0
    , color = Red
    , displaySize = 100
    , debouncedCurrent = 0
    , debounceTimer = AtRest
    }


add : Float -> Meter -> Meter
add amount oldMeter =
    let
        newCurrent =
            oldMeter.current + amount
    in
    { oldMeter | current = newCurrent, debouncedCurrent = newCurrent, debounceTimer = Waiting totalWaitTime }


subtract : Float -> Meter -> Meter
subtract amount oldMeter =
    { oldMeter | current = oldMeter.current - amount, debounceTimer = Waiting totalWaitTime }


setColor : Color -> Meter -> Meter
setColor newColor meter =
    { meter | color = newColor }


setDisplaySize : Int -> Meter -> Meter
setDisplaySize newSize meter =
    { meter | displaySize = newSize }


getCurrent : Meter -> Float
getCurrent { current } =
    current


getDebouncedCurrent : Meter -> Float
getDebouncedCurrent { debouncedCurrent } =
    debouncedCurrent


renderHorizontal : Meter -> Html msg
renderHorizontal { max, current, color, displaySize, debouncedCurrent, debounceTimer } =
    let
        currentPercent =
            current / max

        debouncedCurrentPercent =
            debouncedCurrent / max

        maxPixelWidth =
            toFloat displaySize

        currentPixelWidth =
            maxPixelWidth * currentPercent

        debouncedCurrentPixelWidth =
            maxPixelWidth * debouncedCurrentPercent

        toPx : Float -> String
        toPx =
            round >> String.fromInt >> appendString "px"

        barClass : String
        barClass =
            "h-4 absolute top-0 left-0"

        -- myVar =
        --     Debug.log "stuff" ( currentPercent, debouncedCurrentPercent )
    in
    div
        [ class "h-4 border-2 border-black bg-white overflow-hidden relative"
        , style "width" (toPx maxPixelWidth)
        ]
        [ div [ class barClass, class "bg-red-300", style "width" (toPx debouncedCurrentPixelWidth) ] []
        , div [ class barClass, class (getBgStyle color), style "width" (toPx currentPixelWidth) ] []
        ]


renderVertical : Meter -> Html msg
renderVertical { max, current, color, displaySize } =
    let
        percentFilled =
            current / max

        maxPixelHeight =
            toFloat displaySize

        currentPixelHeight =
            maxPixelHeight * percentFilled

        toPx : Float -> String
        toPx =
            round >> String.fromInt >> appendString "px"
    in
    div
        [ class "w-4 border-2 border-black bg-white overflow-hidden relative"
        , style "height" (toPx maxPixelHeight)
        ]
        [ div [ class "w-4 absolute bottom-0 ", class (getBgStyle color), style "height" (toPx currentPixelHeight) ] []
        ]


isFull : Meter -> Bool
isFull { current, max } =
    current == max


isEmpty : Meter -> Bool
isEmpty { current } =
    current == 0


drain : Meter -> Meter
drain meter =
    { meter | current = 0 }


increment : Float -> Meter -> Meter
increment amount meter =
    let
        newCurrent : Float
        newCurrent =
            clamp 0 meter.max (meter.current + amount)
    in
    { meter | current = newCurrent }


handleAnimationFrameRegen : Float -> Meter -> Meter
handleAnimationFrameRegen delta =
    increment (delta / 100)


handleAnimationFrameDebounce : Float -> Meter -> Meter
handleAnimationFrameDebounce delta meter =
    case meter.debounceTimer of
        AtRest ->
            meter

        Waiting waitTime ->
            let
                newWaitTime =
                    waitTime - delta
            in
            if newWaitTime > 0 then
                { meter | debounceTimer = Waiting newWaitTime }

            else
                let
                    newDelta =
                        -1 * newWaitTime
                in
                -- Slightly confusing recursion here. We zero out the wait time and roll over the excess to the delta and reapply the function
                -- The net effect is we get the right calculations applied to debouncedCurrent and debounceTimer in the next function call in the Tick case
                handleAnimationFrameDebounce newDelta { meter | debounceTimer = Ticking }

        Ticking ->
            let
                percentChange =
                    delta * tickSpeed / 1000

                valChange =
                    meter.max * percentChange

                newDebouncedCurrent =
                    meter.debouncedCurrent - valChange
            in
            if newDebouncedCurrent < meter.current then
                { meter | debouncedCurrent = meter.current, debounceTimer = AtRest }

            else
                { meter | debouncedCurrent = newDebouncedCurrent, debounceTimer = Ticking }
