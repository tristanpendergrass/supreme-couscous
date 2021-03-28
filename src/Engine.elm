port module Engine exposing (Instance, create)

import Browser
import Browser.Events
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra
import Utils


images =
    { selectionArrow = "arrow.png"
    , battleSelection = "battle_selection_2.png"
    }


sounds =
    { select = "rpg_sound_pack/interface/interface1.wav"
    , attack = "rpg_sound_pack/battle/sword-unsheathe.wav"
    }


type alias Instance =
    Program () Model Msg


type alias Input =
    ( Char, String )


type alias EngineArgMove =
    { damage : Int
    , prompt : String
    , inputs : List Input
    }


type alias EngineArgAlly =
    { avatarUrl : String
    , battleUrl : String
    , move : EngineArgMove
    }


type alias EngineArgs =
    { title : String
    , allyOne : EngineArgAlly
    , allyTwo : EngineArgAlly
    , allyThree : EngineArgAlly
    }


create : EngineArgs -> Instance
create engineArgs =
    Browser.element { init = init, update = update engineArgs, view = view engineArgs, subscriptions = subscriptions engineArgs }


getAllyFrom : EngineArgs -> Position -> EngineArgAlly
getAllyFrom engineArgs position =
    case position of
        First ->
            engineArgs.allyOne

        Second ->
            engineArgs.allyTwo

        Third ->
            engineArgs.allyThree



-- MODEL


type Position
    = First
    | Second
    | Third


type alias Selection =
    { position : Position
    , inputs : List Input
    }


createSelection : Position -> Selection
createSelection position =
    { position = position, inputs = [] }


addInputToSelection : Input -> Selection -> Selection
addInputToSelection input selection =
    { selection | inputs = input :: selection.inputs }


isPositionSelected : Maybe Selection -> Position -> Bool
isPositionSelected maybeSelection position =
    case maybeSelection of
        Nothing ->
            False

        Just selection ->
            selection.position == position


type alias Model =
    { allySelection : Maybe Selection }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { allySelection = Nothing }, Cmd.none )



-- UPDATE


port emitSound : String -> Cmd msg


type Msg
    = NoOp
    | SetAllySelection (Maybe Position)
    | Input Input
    | Finish


update : EngineArgs -> Msg -> Model -> ( Model, Cmd Msg )
update engineArgs msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            noOp

        SetAllySelection position ->
            ( { model | allySelection = Maybe.map createSelection position }, emitSound sounds.select )

        Input input ->
            case model.allySelection of
                Nothing ->
                    noOp

                Just selection ->
                    let
                        { position, inputs } =
                            selection

                        nextInput : Maybe Input
                        nextInput =
                            let
                                pattern =
                                    (getAllyFrom engineArgs position).move.inputs
                            in
                            Utils.getNextInput pattern (List.reverse inputs)

                        inputMatches =
                            nextInput
                                |> Maybe.map ((==) input)
                                |> Maybe.withDefault False
                    in
                    if inputMatches then
                        ( { model | allySelection = Maybe.map (addInputToSelection input) model.allySelection }, emitSound sounds.attack )

                    else
                        noOp

        Finish ->
            case model.allySelection of
                Nothing ->
                    noOp

                Just selection ->
                    let
                        {position, inputs} = selection

                        isPatternComplete = 
                            let
                                pattern = (getAllyFrom engineArgs position).move.inputs
                            in
                            Utils.isPatternComplete pattern (List.reverse inputs)
                    in
                    if isPatternComplete then
                        ({ model | allySelection = Nothing}, Cmd.none)

                    else
                        ({ model | allySelection = Nothing}, Cmd.none)





-- SUBSCRIPTIONS


keyDecoder : EngineArgs -> Model -> Decode.Decoder Msg
keyDecoder engineArgs model =
    Decode.map (toUserInput engineArgs model) (Decode.field "key" Decode.string)


tryAll : List (Maybe t) -> Maybe t
tryAll things =
    case things of
        [] ->
            Nothing

        first :: rest ->
            case first of
                Just _ ->
                    first

                Nothing ->
                    tryAll rest


toUserInput : EngineArgs -> Model -> String -> Msg
toUserInput engineArgs model string =
    let
        selectionInput =
            model.allySelection
                |> Maybe.andThen
                    (\{ position } ->
                        let
                            ally =
                                getAllyFrom engineArgs position
                        in
                        toSelectedAllyInput ally.move.inputs string
                    )

        result =
            tryAll
                [ toGlobalUserInput string
                , selectionInput
                ]
    in
    Maybe.withDefault NoOp result


toSelectedAllyInput : List Input -> String -> Maybe Msg
toSelectedAllyInput inputs string =
    let
        inputMatchesString : String -> Input -> Bool
        inputMatchesString iterString ( trigger, _ ) =
            iterString == String.fromChar trigger
    in
    inputs
        |> List.Extra.find (inputMatchesString string)
        |> Maybe.map Input


toGlobalUserInput : String -> Maybe Msg
toGlobalUserInput string =
    case string of
        "1" ->
            Just (SetAllySelection (Just First))

        "2" ->
            Just (SetAllySelection (Just Second))

        "3" ->
            Just (SetAllySelection (Just Third))

        "Escape" ->
            Just (SetAllySelection Nothing)

        "q" ->
            Just (SetAllySelection Nothing)

        _ ->
            Nothing


subscriptions : EngineArgs -> Model -> Sub Msg
subscriptions engineArgs model =
    Sub.batch
        [ Browser.Events.onKeyUp (keyDecoder engineArgs model) ]



-- VIEW


renderTop : EngineArgs -> Model -> Html Msg
renderTop engineArgs model =
    let
        renderAlly : Bool -> String -> Msg -> Html Msg
        renderAlly isSelected imageUrl selectionMsg =
            div [ class "relative" ]
                [ img [ class "w-24 h-24", src imageUrl, onClick selectionMsg ] []
                , if isSelected then
                    img [ class "absolute inline-block w-24 h-24 top-0 left-0", src images.battleSelection ] []

                  else
                    div [] []
                ]

        renderAllies =
            div [ class "border border-dashed h-full w-96 flex-col" ]
                [ div [ class "w-full h-1/3 flex items-center" ]
                    [ div
                        [ class "ml-4" ]
                        [ renderAlly (isPositionSelected model.allySelection First) engineArgs.allyOne.battleUrl (SetAllySelection (Just First))
                        ]
                    ]
                , div [ class "w-full h-1/3 flex items-center" ]
                    [ div
                        [ class "ml-32" ]
                        [ renderAlly (isPositionSelected model.allySelection Second) engineArgs.allyTwo.battleUrl (SetAllySelection (Just Second)) ]
                    ]
                , div [ class "w-full h-1/3 flex items-center" ]
                    [ div
                        [ class "ml-4" ]
                        [ renderAlly (isPositionSelected model.allySelection Third) engineArgs.allyThree.battleUrl (SetAllySelection (Just Third))
                        ]
                    ]
                ]

        renderEnemy =
            div [ class "border border-dashed border-red-500 h-full w-96" ]
                [ div [ class "w-full h-full flex items-center" ]
                    [ div [ class "w-24 h-24 rounded-full bg-red-600 mr-12" ] []
                    ]
                ]
    in
    div [ class "w-full h-full bg-blue-400 flex justify-between" ]
        [ renderAllies
        , renderEnemy
        ]


renderBottom : EngineArgs -> Model -> Html Msg
renderBottom engineArgs model =
    let
        avatarUrlForPosition : Position -> String
        avatarUrlForPosition position =
            position
                |> getAllyFrom engineArgs
                |> .avatarUrl

        promptForPosition : Position -> String
        promptForPosition position =
            position
                |> getAllyFrom engineArgs
                |> (.move >> .prompt)

        moveForPosition : Position -> EngineArgMove
        moveForPosition position =
            position
                |> getAllyFrom engineArgs
                |> .move

        renderPortrait : Maybe String -> Html Msg
        renderPortrait maybeUrl =
            div [ class "overflow-hidden w-48 h-48 relative" ]
                [ case maybeUrl of
                    Just url ->
                        div [ class "bg-blue-200 border-4 border-gray-900" ] [ img [ src url, class "bg-blue-200" ] [] ]

                    Nothing ->
                        div [] []
                ]

        renderPrompt : Maybe String -> Html Msg
        renderPrompt maybePrompt =
            div [ class "w-64 h-48" ]
                [ case maybePrompt of
                    Just prompt ->
                        div [ class "italic" ] [ text prompt ]

                    Nothing ->
                        div [] []
                ]

        renderInput : Input -> Html Msg
        renderInput input =
            let
                ( trigger, name ) =
                    input
            in
            div [ class "flex items-center h-8 border-2 border-gray-900 mb-2 cursor-pointer", onClick (Input input) ]
                [ div [ class "py-1 px-2 bg-gray-900 text-gray-100" ] [ text <| String.fromChar trigger ]
                , div [ class "py-1 px-2" ] [ text name ]
                ]

        renderFinish : EngineArgMove -> Html Msg
        renderFinish move =
            button [ onClick Finish ] [ text "Finish" ]

        renderMove : Maybe EngineArgMove -> Html Msg
        renderMove maybeMove =
            div [ class "w-96 h-48 " ]
                [ case maybeMove of
                    Just move ->
                        div [ class "h-full w-full flex-col" ]
                            [ div [ class "w-96 h-40" ]
                                [ move.inputs
                                    |> List.Extra.unique
                                    |> List.map renderInput
                                    |> div [ class "flex space-x-2 flex-wrap" ]
                                ]
                            , div [ class "w-96 h-8" ] [ renderFinish move ]
                            ]

                    Nothing ->
                        div [] []
                ]

        renderDoneInput : Input -> Html Msg
        renderDoneInput ( _, move ) =
            div [] [ text move ]

        renderInputs : Maybe (List Input) -> Html Msg
        renderInputs maybeInputs =
            div [ class "flex-grow h-48 border border-gray-900" ]
                [ case maybeInputs of
                    Just inputs ->
                        div [ class "flex-col" ] (List.map renderDoneInput inputs)

                    Nothing ->
                        div [] []
                ]
    in
    div [ class "w-full h-full border-gray-500 border-4 bg-gray-400 flex items-center p-2 space-x-2" ]
        [ renderPortrait <| Maybe.map (.position >> avatarUrlForPosition) model.allySelection
        , renderPrompt <| Maybe.map (.position >> promptForPosition) model.allySelection
        , renderMove <| Maybe.map (.position >> moveForPosition) model.allySelection
        , renderInputs <| Maybe.map .inputs model.allySelection
        ]


view : EngineArgs -> Model -> Html Msg
view engineArgs model =
    div [ class "bg-gray-900 w-screen h-screen flex items-center justify-center" ]
        [ div
            [ class "rounded border-gray-100 border-4"
            , style "width" "64rem" -- i.e. w-256
            , style "height" "42rem" -- i.e. h-168
            ]
            [ div [ class "w-full h-2/3" ] [ renderTop engineArgs model ]
            , div [ class "w-full h-1/3" ] [ renderBottom engineArgs model ]
            ]
        ]
