port module Engine exposing (EngineArgAlly, EngineArgEnemy, EngineArgMove, Instance, create, damage)

import Browser
import Browser.Events
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra
import Meter exposing (Meter)
import SelectionList exposing (SelectionList)
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


type EngineArgEffect
    = Damage Int


type alias EngineArgMove =
    { onSuccess : List EngineArgEffect
    , prompt : String
    , inputs : List Input
    }


type alias EngineArgAlly =
    { avatarUrl : String
    , battleUrl : String
    , move : EngineArgMove
    , maxHealth : Int
    }


type alias EngineArgEnemy =
    { battleUrl : String
    , maxHealth : Int
    }


type alias EngineArgs =
    { title : String
    , initialParty : List EngineArgAlly
    , initialEnemy : EngineArgEnemy
    }


create : EngineArgs -> Instance
create engineArgs =
    Browser.element { init = init engineArgs, update = update engineArgs, view = view engineArgs, subscriptions = subscriptions engineArgs }



-- MODEL


damage : Int -> EngineArgEffect
damage =
    Damage


type alias Selection =
    { liveInputs : List Input
    }


addInputToSelection : Input -> Selection -> Selection
addInputToSelection input selection =
    { selection | liveInputs = input :: selection.liveInputs }


type alias GameAlly =
    { stats : EngineArgAlly
    , health : Meter
    }


type alias Party =
    SelectionList GameAlly Selection


type alias Enemy =
    { stats : EngineArgEnemy
    , health : Meter
    }


type alias Model =
    { party : Party
    , enemy : Enemy
    }


init : EngineArgs -> () -> ( Model, Cmd Msg )
init engineArgs _ =
    let
        createAlly : EngineArgAlly -> GameAlly
        createAlly stats =
            { stats = stats
            , health = Meter.create { current = toFloat stats.maxHealth, max = toFloat stats.maxHealth }
            }

        initialSelectionList =
            engineArgs.initialParty
                |> List.map createAlly
                |> SelectionList.create

        initialEnemy =
            { stats = engineArgs.initialEnemy
            , health =
                Meter.create
                    { max = toFloat engineArgs.initialEnemy.maxHealth
                    , current = toFloat engineArgs.initialEnemy.maxHealth
                    }
            }
    in
    ( { party = initialSelectionList, enemy = initialEnemy }, Cmd.none )



-- UPDATE


port emitSound : String -> Cmd msg


type Msg
    = NoOp
    | SelectAlly (Maybe Int)
    | Input Input
    | Finish


updateParty : (Party -> Party) -> Model -> Model
updateParty updateFn model =
    { model | party = updateFn model.party }


getCompletedAlly : Model -> Maybe GameAlly
getCompletedAlly model =
    SelectionList.getSelected model.party
        |> Maybe.andThen
            (\( selectedAlly, { liveInputs } ) ->
                if Utils.isPatternComplete selectedAlly.stats.move.inputs (List.reverse liveInputs) then
                    Just selectedAlly

                else
                    Nothing
            )


isPatternComplete : Model -> Bool
isPatternComplete model =
    case getCompletedAlly model of
        Just _ ->
            True

        Nothing ->
            False


dealDamageToEnemy : Int -> Model -> Model
dealDamageToEnemy amount model =
    let
        oldEnemy =
            model.enemy

        newEnemy : Enemy
        newEnemy =
            { oldEnemy | health = Meter.subtract (toFloat amount) oldEnemy.health }
    in
    { model | enemy = newEnemy }


update : EngineArgs -> Msg -> Model -> ( Model, Cmd Msg )
update engineArgs msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            noOp

        SelectAlly maybePosition ->
            case maybePosition of
                Nothing ->
                    -- Deselection
                    let
                        newModel =
                            model
                                |> updateParty SelectionList.clearSelection
                    in
                    ( newModel, Cmd.none )

                Just position ->
                    -- Selection of ally
                    let
                        updatePosition : Party -> Party
                        updatePosition party =
                            SelectionList.select position { liveInputs = [] } party
                                |> Result.withDefault party

                        newModel =
                            model
                                |> updateParty updatePosition
                    in
                    ( newModel, emitSound sounds.select )

        Input input ->
            let
                updateInputs : ( GameAlly, Selection ) -> Selection
                updateInputs ( selectedAlly, selection ) =
                    let
                        nextInput : Maybe Input
                        nextInput =
                            let
                                pattern =
                                    selectedAlly.stats.move.inputs
                            in
                            Utils.getNextInput pattern (List.reverse selection.liveInputs)

                        inputMatches =
                            nextInput
                                |> Maybe.map ((==) input)
                                |> Maybe.withDefault False
                    in
                    if inputMatches then
                        addInputToSelection input selection

                    else
                        selection
            in
            case SelectionList.mapSelectionData updateInputs model.party of
                Err _ ->
                    noOp

                Ok newParty ->
                    ( { model | party = newParty }, emitSound sounds.attack )

        Finish ->
            case getCompletedAlly model of
                Nothing ->
                    -- Finish was called without a completed pattern for a selected ally
                    let
                        newModel =
                            model
                                |> updateParty SelectionList.clearSelection
                    in
                    ( newModel, emitSound sounds.select )

                Just selectedAlly ->
                    let
                        applyOnSuccess : Model -> Model
                        applyOnSuccess oldModel =
                            let
                                effects =
                                    selectedAlly.stats.move.onSuccess

                                applyEffect : EngineArgEffect -> Model -> Model
                                applyEffect effect m =
                                    case effect of
                                        Damage amount ->
                                            dealDamageToEnemy amount m
                            in
                            List.foldl applyEffect oldModel effects

                        newModel =
                            model
                                |> applyOnSuccess
                                |> updateParty SelectionList.clearSelection
                    in
                    ( newModel, emitSound sounds.attack )



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
            SelectionList.getSelected model.party
                |> Maybe.andThen
                    (\( selectedAlly, _ ) ->
                        toSelectedAllyInput selectedAlly.stats.move.inputs string
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
            Just (SelectAlly (Just 0))

        "2" ->
            Just (SelectAlly (Just 1))

        "3" ->
            Just (SelectAlly (Just 2))

        "Escape" ->
            Just (SelectAlly Nothing)

        "q" ->
            Just (SelectAlly Nothing)

        "Enter" ->
            Just Finish

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
                (SelectionList.mapItems
                    (\isSelected index ally ->
                        div [ class "w-full h-1/3 flex items-center" ]
                            [ div
                                [ class "ml-4" ]
                                [ renderAlly isSelected ally.stats.battleUrl (SelectAlly (Just index))
                                ]
                            ]
                    )
                    model.party
                )

        renderEnemy : Enemy -> Html Msg
        renderEnemy enemy =
            div [ class "border border-dashed border-red-500 h-full w-96" ]
                [ div [ class "w-full h-full flex items-center justify-center flex-col space-y-2" ]
                    [ img [ class "inline-block h-48", src enemy.stats.battleUrl ] []
                    , Meter.render 150 enemy.health
                    ]
                ]
    in
    div [ class "w-full h-full bg-blue-400 flex justify-between" ]
        [ renderAllies
        , renderEnemy model.enemy
        ]


renderBottom : EngineArgs -> Model -> Html Msg
renderBottom engineArgs model =
    let
        renderPortrait : Party -> Html Msg
        renderPortrait party =
            div [ class "overflow-hidden w-48 h-48 relative" ]
                [ case SelectionList.getSelected party of
                    Just ( selectedAlly, _ ) ->
                        div [ class "bg-blue-200 border-4 border-gray-900" ] [ img [ src selectedAlly.stats.avatarUrl, class "bg-blue-200" ] [] ]

                    Nothing ->
                        div [] []
                ]

        renderPrompt : Party -> Html Msg
        renderPrompt party =
            div [ class "w-64 h-48" ]
                [ case SelectionList.getSelected party of
                    Just ( selectedAlly, _ ) ->
                        div [ class "italic" ] [ text selectedAlly.stats.move.prompt ]

                    Nothing ->
                        div [] []
                ]

        inputContainer =
            "flex items-center h-8 border-2 border-gray-900 mb-2 cursor-pointer"

        inputTrigger =
            "py-1 px-2 bg-gray-900 text-gray-100"

        inputLabel =
            "py-1 px-2"

        renderInput : Input -> Html Msg
        renderInput input =
            let
                ( trigger, name ) =
                    input
            in
            button [ class inputContainer, onClick (Input input) ]
                [ div [ class inputTrigger ] [ text <| String.fromChar trigger ]
                , div [ class inputLabel ] [ text name ]
                ]

        renderFinish : EngineArgMove -> Html Msg
        renderFinish _ =
            button [ class inputContainer, onClick Finish ]
                [ div [ class inputTrigger ] [ text "Enter" ]
                , div [ class inputLabel ] [ text "Finish" ]
                ]

        renderMove : Party -> Html Msg
        renderMove party =
            div [ class "w-96 h-48 " ]
                [ case SelectionList.getSelected party of
                    Just ( selectedAlly, _ ) ->
                        div [ class "h-full w-full flex-col" ]
                            [ div [ class "w-96 h-40" ]
                                [ selectedAlly.stats.move.inputs
                                    |> List.Extra.unique
                                    |> List.map renderInput
                                    |> div [ class "flex space-x-2 flex-wrap" ]
                                ]
                            , div [ class "w-96 h-8" ] [ renderFinish selectedAlly.stats.move ]
                            ]

                    Nothing ->
                        div [] []
                ]

        renderLiveInput : Input -> Html Msg
        renderLiveInput ( _, move ) =
            div [] [ text move ]

        renderInputs : Party -> Html Msg
        renderInputs party =
            case SelectionList.getSelected party of
                Just ( _, { liveInputs } ) ->
                    div [ class "flex-grow h-48 border border-gray-900" ]
                        [ div [ class "flex-col" ] (List.map renderLiveInput liveInputs)
                        ]

                Nothing ->
                    div [] []
    in
    div [ class "w-full h-full border-gray-500 border-4 bg-gray-400 flex items-center p-2 space-x-2" ]
        [ renderPortrait <| model.party
        , renderPrompt <| model.party
        , renderMove <| model.party
        , renderInputs <| model.party
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
