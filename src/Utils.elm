module Utils exposing (getNextInput, isPatternComplete)

-- iterate over pattern, keeping a pointer to liveInputs as well.
-- To move forward in pattern you must find the matching input in liveInputs, moving strictly forward.
-- If the end of pattern is reached then return Nothing
-- If the end of liveInputs is reached then return the current item from pattern iteration


getNextInput : List a -> List a -> Maybe a
getNextInput pattern liveInputs =
    case ( pattern, liveInputs ) of
        ( [], _ ) ->
            Nothing

        ( next :: _, [] ) ->
            Just next

        ( nextPattern :: restPattern, nextLive :: restLive ) ->
            if nextPattern == nextLive then
                getNextInput restPattern restLive

            else
                getNextInput pattern restLive


isPatternComplete : List a -> List a -> Bool
isPatternComplete pattern liveInputs =
    case ( pattern, liveInputs ) of
        ( [], _ ) ->
            True

        ( _, [] ) ->
            False

        ( nextPattern :: restPattern, nextLive :: restLive ) ->
            if nextPattern == nextLive then
                isPatternComplete restPattern restLive

            else
                isPatternComplete pattern restLive
