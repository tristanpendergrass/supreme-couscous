module Meter exposing (Meter, add, create, render, subtract)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


type Meter
    = Meter { max : Float, current : Float }


create : { max : Float, current : Float } -> Meter
create params =
    Meter params


add : Float -> Meter -> Meter
add amount (Meter { max, current }) =
    Meter { max = max, current = current + amount }


subtract : Float -> Meter -> Meter
subtract amount (Meter { max, current }) =
    Meter { max = max, current = current - amount }


render : Int -> Meter -> Html msg
render width (Meter { max, current }) =
    let
        percentFilled =
            current / max

        maxPixelWidth =
            toFloat width

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
        [ div [ class "h-8 bg-red-500", style "width" (toPx currentPixelWidth) ] []
        ]
