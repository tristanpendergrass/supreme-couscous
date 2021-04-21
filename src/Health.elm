module Health exposing (Health, add, create, render, subtract)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


type Health
    = Health { max : Int, current : Int }


create : { max : Int, current : Int } -> Health
create params =
    Health params


add : Int -> Health -> Health
add amount (Health { max, current }) =
    Health { max = max, current = current + amount }


subtract : Int -> Health -> Health
subtract amount (Health { max, current }) =
    Health { max = max, current = current - amount }


render : Int -> Health -> Html msg
render width (Health { max, current }) =
    let
        percentFilled =
            toFloat current / toFloat max

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
