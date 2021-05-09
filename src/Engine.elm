port module Engine exposing (EngineArgEnemy, Instance, create)

import Ally exposing (Ally)
import Animation exposing (Animation)
import Browser
import Browser.Events
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra
import Meter exposing (Meter)
import Party exposing (Party, Selection)
import Random
import Utils


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Nothing ->
            False

        _ ->
            True


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


type alias EngineArgEnemy =
    { battleUrl : String
    , maxHealth : Int
    , maxEnergy : Int
    , damage : Int
    }


type alias EngineArgs =
    { title : String
    , initialParty : List Ally.Stats
    , initialEnemy : EngineArgEnemy
    }


create : EngineArgs -> Instance
create engineArgs =
    Browser.element { init = init engineArgs, update = update engineArgs, view = view engineArgs, subscriptions = subscriptions engineArgs }



-- MODEL


addInputToSelection : Input -> Selection -> Selection
addInputToSelection input selection =
    { selection | liveInputs = input :: selection.liveInputs }


type alias Enemy =
    { stats : EngineArgEnemy
    , health : Meter
    , energy : Meter
    , spriteAnimation : Maybe Animation
    }


type alias Model =
    { seed : Random.Seed
    , party : Party
    , enemy : Enemy
    }


init : EngineArgs -> () -> ( Model, Cmd Msg )
init engineArgs _ =
    let
        initialParty =
            engineArgs.initialParty
                |> List.map Ally.create
                |> Party.create

        initialEnemy : Enemy
        initialEnemy =
            { stats = engineArgs.initialEnemy
            , health = Meter.create (toFloat engineArgs.initialEnemy.maxHealth)
            , energy = Meter.create (toFloat engineArgs.initialEnemy.maxEnergy) |> Meter.drain
            , spriteAnimation = Nothing
            }
    in
    ( { seed = Random.initialSeed 0
      , party = initialParty
      , enemy = initialEnemy
      }
    , Cmd.none
    )



-- UPDATE


port emitSound : String -> Cmd msg


type Msg
    = NoOp
    | SelectAlly (Maybe Int)
    | Input Input
    | Finish
    | HandleAnimationFrame Float


updateParty : (Party -> Party) -> Model -> Model
updateParty updateFn model =
    { model | party = updateFn model.party }


damageRandomAlly : Float -> Model -> Model
damageRandomAlly damageAmount oldModel =
    let
        ( newParty, newSeed ) =
            Random.step (Party.damageRandomMember damageAmount oldModel.party) oldModel.seed
    in
    { oldModel | seed = newSeed, party = newParty }


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


updateEnemy : (Enemy -> Enemy) -> Model -> Model
updateEnemy updateFn model =
    { model | enemy = updateFn model.enemy }


updateEnemyEnergy : Float -> Enemy -> Enemy
updateEnemyEnergy delta enemy =
    { enemy | energy = Meter.handleAnimationFrame delta enemy.energy }


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
                                |> updateParty Party.clearSelection
                    in
                    ( newModel, Cmd.none )

                Just position ->
                    -- Selection of ally
                    let
                        allyHasEnergy : Bool
                        allyHasEnergy =
                            Party.getLiveAllyAt position model.party
                                |> Maybe.map (.energy >> Meter.isFull)
                                |> Maybe.withDefault False
                    in
                    if allyHasEnergy then
                        let
                            updatePosition : Party -> Party
                            updatePosition party =
                                if allyHasEnergy then
                                    Party.select position { liveInputs = [] } party
                                        |> Result.withDefault party

                                else
                                    party

                            newModel =
                                model
                                    |> updateParty updatePosition
                        in
                        ( newModel, emitSound sounds.select )

                    else
                        noOp

        Input input ->
            let
                updateInputs : ( Ally, Selection ) -> ( Ally, Selection )
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
                        ( selectedAlly, addInputToSelection input selection )

                    else
                        ( selectedAlly, selection )
            in
            case Party.mapSelection updateInputs model.party of
                Err _ ->
                    noOp

                Ok newParty ->
                    ( { model | party = newParty }, emitSound sounds.attack )

        Finish ->
            case Party.getSelectedAllyIfComplete model.party of
                Nothing ->
                    -- Finish was called without a completed pattern for a selected ally
                    let
                        newModel =
                            model
                                |> updateParty Party.clearSelection
                    in
                    ( newModel, emitSound sounds.select )

                Just selectedAlly ->
                    let
                        applyOnSuccess : Model -> Model
                        applyOnSuccess oldModel =
                            let
                                effects =
                                    selectedAlly.stats.move.onSuccess

                                applyEffect : Ally.Effect -> Model -> Model
                                applyEffect effect m =
                                    case effect of
                                        Ally.Damage amount ->
                                            dealDamageToEnemy amount m
                            in
                            List.foldl applyEffect oldModel effects

                        drainMeter : Ally -> Ally
                        drainMeter ally =
                            { ally | energy = Meter.drain ally.energy }

                        updateEnergy : Party -> Party
                        updateEnergy party =
                            party
                                |> Party.mapSelection
                                    (\( ally, selectionData ) ->
                                        ( drainMeter ally, selectionData )
                                    )
                                |> Result.withDefault party

                        newModel =
                            model
                                |> applyOnSuccess
                                |> updateParty updateEnergy
                                |> updateParty Party.clearSelection
                                |> updateEnemy (\enemy -> { enemy | spriteAnimation = Just <| Animation.create Animation.Shake })
                    in
                    ( newModel, emitSound sounds.attack )

        HandleAnimationFrame delta ->
            let
                updateAnimation : { a | spriteAnimation : Maybe Animation } -> { a | spriteAnimation : Maybe Animation }
                updateAnimation obj =
                    let
                        newAnimation =
                            obj.spriteAnimation
                                |> Maybe.andThen (Animation.updateAnimation delta)
                    in
                    { obj | spriteAnimation = newAnimation }

                handleEnemyAttack : Model -> Model
                handleEnemyAttack oldModel =
                    let
                        enemy =
                            oldModel.enemy

                        readyToAttack =
                            Meter.isFull enemy.energy
                    in
                    if readyToAttack then
                        let
                            drainEnergy : Enemy -> Enemy
                            drainEnergy oldEnemy =
                                { enemy | energy = Meter.drain oldEnemy.energy }

                            updateAlly : Ally -> Ally
                            updateAlly =
                                Ally.removeHealth (toFloat enemy.stats.damage) >> Ally.addShake
                        in
                        oldModel
                            |> damageRandomAlly (toFloat enemy.stats.damage)
                            |> updateEnemy drainEnergy
                            |> updateEnemy (updateEnemyEnergy delta)

                    else
                        oldModel
                            |> updateEnemy (updateEnemyEnergy delta)

                newModel =
                    model
                        |> updateParty (Party.handleAnimationFrame delta)
                        |> handleEnemyAttack
                        |> updateEnemy updateAnimation
            in
            ( newModel, Cmd.none )



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
            Party.getSelected model.party
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
        [ Browser.Events.onKeyUp (keyDecoder engineArgs model)
        , Browser.Events.onAnimationFrameDelta HandleAnimationFrame
        ]



-- VIEW


inputContainer : String
inputContainer =
    "flex items-center h-8 border-2 border-gray-900 cursor-pointer"


inputTrigger : String
inputTrigger =
    "py-1 px-2 bg-gray-900 text-gray-100"


inputLabel : String
inputLabel =
    "py-1 px-2"


renderTop : Model -> Html Msg
renderTop model =
    let
        allyStatsContainer =
            "w-8"

        renderAliveAlly : Bool -> Ally -> Msg -> Int -> Html Msg
        renderAliveAlly isSelected ally selectionMsg index =
            let
                { stats, health, energy } =
                    ally

                { battleUrl } =
                    stats

                isAnyAllySelected =
                    model.party
                        |> Party.getSelected
                        |> isJust

                allyCanBeSelected =
                    not isAnyAllySelected
                        && Meter.isFull ally.energy

                allyStats =
                    div [ class allyStatsContainer, class "flex space-x-1" ]
                        [ energy
                            |> Meter.setColor Meter.Blue
                            |> Meter.setDisplaySize 75
                            |> Meter.renderVertical
                        , health
                            |> Meter.setDisplaySize 75
                            |> Meter.renderVertical
                        ]

                allyImage =
                    div [ class "relative" ]
                        [ img [ class "w-24 h-24", class (Animation.classForAnimation ally.spriteAnimation), src battleUrl ] []
                        , if isSelected then
                            img [ class "absolute inline-block w-24 h-24 top-0 left-0", src images.battleSelection ] []

                          else
                            div [] []
                        ]

                allySelector =
                    div
                        [ class inputContainer
                        , class <|
                            if allyCanBeSelected then
                                ""

                            else
                                "invisible"
                        , onClick selectionMsg
                        ]
                        [ div [ class inputTrigger ] [ text <| String.fromInt (index + 1) ]
                        , div [ class inputLabel ] [ text "Select" ]
                        ]
            in
            div [ class "flex-col space-y-2" ]
                [ div [ class "flex w-full justify-center" ]
                    [ allySelector ]
                , div [ class "flex items-end space-x-2" ]
                    [ div [ class "mb-2" ] [ allyStats ], allyImage ]
                ]

        renderDeadAlly : Ally.Stats -> Html Msg
        renderDeadAlly stats =
            div [ class "flex-col space-y-2" ]
                [ div [ class "flex items-end space-x-2" ]
                    [ div [ class "mb-2" ] [ div [ class allyStatsContainer ] [] ]
                    , div [ class "relative" ]
                        [ img [ class "w-24 h-24", src stats.tombstoneUrl ] []
                        ]
                    ]
                ]

        renderAllies =
            div [ class "border border-dashed h-full w-96 flex-col" ]
                (model.party
                    |> Party.toListWithSelectionStatus
                    |> List.indexedMap
                        (\index ( allySpot, isSelected ) ->
                            div [ class "w-full h-1/3 flex items-center" ]
                                [ div
                                    [ class "ml-4" ]
                                    [ case allySpot of
                                        Party.AliveAlly ally ->
                                            renderAliveAlly isSelected ally (SelectAlly (Just index)) index

                                        Party.DeadAlly stats ->
                                            renderDeadAlly stats
                                    ]
                                ]
                        )
                )

        renderEnemy : Enemy -> Html Msg
        renderEnemy enemy =
            div [ class "border border-dashed border-red-500 h-full w-96" ]
                [ div [ class "w-full h-full flex items-center justify-center flex-col space-y-2" ]
                    [ img [ class "inline-block h-48", class <| Animation.classForAnimation enemy.spriteAnimation, src enemy.stats.battleUrl ] []
                    , enemy.health
                        |> Meter.setDisplaySize 150
                        |> Meter.renderHorizontal
                    ]
                ]
    in
    div [ class "w-full h-full bg-blue-400 flex justify-between" ]
        [ renderAllies
        , renderEnemy model.enemy
        ]


renderBottom : Model -> Html Msg
renderBottom model =
    let
        renderPortrait : Party -> Html Msg
        renderPortrait party =
            div [ class "overflow-hidden w-48 h-48 relative" ]
                [ case Party.getSelected party of
                    Just ( selectedAlly, _ ) ->
                        div [ class "bg-blue-200 border-4 border-gray-900" ] [ img [ src selectedAlly.stats.avatarUrl, class "bg-blue-200" ] [] ]

                    Nothing ->
                        div [] []
                ]

        renderPrompt : Party -> Html Msg
        renderPrompt party =
            div [ class "w-64 h-48" ]
                [ case Party.getSelected party of
                    Just ( selectedAlly, _ ) ->
                        div [ class "italic" ] [ text selectedAlly.stats.move.prompt ]

                    Nothing ->
                        div [] []
                ]

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

        renderFinish : Ally.Move -> Html Msg
        renderFinish _ =
            button [ class inputContainer, onClick Finish ]
                [ div [ class inputTrigger ] [ text "Enter" ]
                , div [ class inputLabel ] [ text "Finish" ]
                ]

        renderMove : Party -> Html Msg
        renderMove party =
            div [ class "w-96 h-48 " ]
                [ case Party.getSelected party of
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
            case Party.getSelected party of
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
            [ div [ class "w-full h-2/3" ] [ renderTop model ]
            , div [ class "w-full h-1/3" ] [ renderBottom model ]
            ]
        ]
