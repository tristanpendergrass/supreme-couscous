module Meter exposing
    ( Color(..)
    , Meter
    , add
    , create
    , drain
    , getCurrent
    , handleAnimationFrame
    , isFull
    , renderHorizontal
    , renderVertical
    , setColor
    , setDisplaySize
    , subtract
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


type Color
    = Red
    | Blue


getBgStyle : Color -> String
getBgStyle color =
    case color of
        Red ->
            "bg-red-500"

        Blue ->
            "bg-blue-700"


type alias Meter =
    { max : Float, current : Float, color : Color, displaySize : Int }


create : Float -> Meter
create max =
    { max = max, current = max, color = Red, displaySize = 100 }


add : Float -> Meter -> Meter
add amount oldMeter =
    { oldMeter | current = oldMeter.current + amount }


subtract : Float -> Meter -> Meter
subtract amount oldMeter =
    { oldMeter | current = oldMeter.current - amount }


setColor : Color -> Meter -> Meter
setColor newColor meter =
    { meter | color = newColor }


setDisplaySize : Int -> Meter -> Meter
setDisplaySize newSize meter =
    { meter | displaySize = newSize }


getCurrent : Meter -> Float
getCurrent { current } =
    current


renderHorizontal : Meter -> Html msg
renderHorizontal { max, current, color, displaySize } =
    let
        percentFilled =
            current / max

        maxPixelWidth =
            toFloat displaySize

        currentPixelWidth =
            maxPixelWidth * percentFilled

        appendString second first =
            first ++ second

        toPx : Float -> String
        toPx =
            round >> String.fromInt >> appendString "px"
    in
    div
        [ class "h-4 border-2 border-black bg-white overflow-hidden"
        , style "width" (toPx maxPixelWidth)
        ]
        [ div [ class "h-4", class (getBgStyle color), style "width" (toPx currentPixelWidth) ] []
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

        appendString second first =
            first ++ second

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


handleAnimationFrame : Float -> Meter -> Meter
handleAnimationFrame delta meter =
    increment (delta / 100) meter
