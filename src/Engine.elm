port module Engine exposing (EngineArgEnemy, Instance, create)

import ActionTimer exposing (ActionTimer)
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
import Party exposing (Party)
import Random
import SelectionList exposing (SelectionList)
import Utils


allyActionTimings : ActionTimer.Timings
allyActionTimings =
    { slideOutTime = 500
    , stayTime = 2000
    , slideInTime = 10000
    }


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


type alias Selection =
    { liveInputs : List Input
    }


type alias Enemy =
    { stats : EngineArgEnemy
    , health : Meter
    , energy : Meter
    , spriteAnimation : Maybe Animation
    }


type Action
    = AllyMove ActionTimer Ally Ally.Move
    | EnemyMove ActionTimer


type alias Game =
    { seed : Random.Seed
    , party : Party
    , enemy : Enemy
    , actions : SelectionList Action Selection
    , health : Meter
    }


type Model
    = GameInProgress Game
    | GameWon Game
    | GameLost Game


init : EngineArgs -> () -> ( Model, Cmd Msg )
init engineArgs _ =
    let
        initialParty : Party
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

        initialActions : SelectionList Action Selection
        initialActions =
            SelectionList.create 5
    in
    ( GameInProgress
        { seed = Random.initialSeed 0
        , party = initialParty
        , enemy = initialEnemy
        , actions = initialActions
        , health = Meter.create 100
        }
    , Cmd.none
    )



-- UPDATE


port emitSound : String -> Cmd msg


type Msg
    = NoOp
    | SelectAction Int
    | DeselectAction
    | Input Input
    | Finish
    | HandleAnimationFrame Float


updateParty : (Party -> Party) -> Game -> Game
updateParty updateFn game =
    { game | party = updateFn game.party }


updateActions : (SelectionList Action Selection -> SelectionList Action Selection) -> Game -> Game
updateActions fn game =
    { game | actions = fn game.actions }


dealDamageToEnemy : Int -> Game -> Game
dealDamageToEnemy amount model =
    let
        oldEnemy =
            model.enemy

        newEnemy : Enemy
        newEnemy =
            { oldEnemy | health = Meter.subtract (toFloat amount) oldEnemy.health }
    in
    { model | enemy = newEnemy }


updateEnemy : (Enemy -> Enemy) -> Game -> Game
updateEnemy fn game =
    { game | enemy = fn game.enemy }


updateHealth : (Meter -> Meter) -> Game -> Game
updateHealth fn game =
    { game | health = fn game.health }


updateEnemyEnergy : Float -> Enemy -> Enemy
updateEnemyEnergy delta enemy =
    { enemy | energy = Meter.handleAnimationFrameRegen delta enemy.energy }


addAction : Action -> Game -> Game
addAction action =
    updateActions (SelectionList.push action)


setSeed : Random.Seed -> Game -> Game
setSeed seed game =
    { game | seed = seed }


addNewAllyActions : Game -> Game
addNewAllyActions =
    let
        addAllyAction : Ally -> Game -> Game
        addAllyAction ally accumGame =
            if Ally.energyIsFull ally then
                case ally.stats.moves of
                    [] ->
                        accumGame

                    first :: rest ->
                        let
                            randomMove =
                                Random.uniform first rest

                            ( move, newSeed ) =
                                Random.step randomMove accumGame.seed

                            actionTimer =
                                ActionTimer.create allyActionTimings
                        in
                        accumGame
                            |> addAction (AllyMove actionTimer ally move)
                            |> setSeed newSeed

            else
                accumGame

        addActions : Game -> Game
        addActions oldGame =
            oldGame.party
                |> Party.getAliveAllies
                |> List.foldl addAllyAction oldGame

        removeEnergy : Game -> Game
        removeEnergy =
            updateParty (Party.mapAliveAllies Ally.drainEnergyIfFull)
    in
    addActions >> removeEnergy


getTimer : Action -> ActionTimer
getTimer action =
    case action of
        AllyMove actionTimer _ _ ->
            actionTimer

        EnemyMove actionTimer ->
            actionTimer


removeDoneActions : Game -> Game
removeDoneActions game =
    game
        |> updateActions (SelectionList.filterUnselected (getTimer >> ActionTimer.isDone >> not))


applyOnSuccess : Ally.Move -> Game -> Game
applyOnSuccess move game =
    let
        effects =
            move.onSuccess

        applyEffect : Ally.Effect -> Game -> Game
        applyEffect effect g =
            case effect of
                Ally.Damage amount ->
                    dealDamageToEnemy amount g
    in
    List.foldl applyEffect game effects


update : EngineArgs -> Msg -> Model -> ( Model, Cmd Msg )
update engineArgs msg model =
    case model of
        GameInProgress game ->
            case updateGame engineArgs msg game of
                ContinueGame ( newGame, commands ) ->
                    ( GameInProgress newGame, commands )

                PlayerWon ( newGame, commands ) ->
                    ( GameWon newGame, commands )

                PlayerLost ( newGame, commands ) ->
                    ( GameLost newGame, commands )

        _ ->
            ( model, Cmd.none )


type GameUpdate
    = ContinueGame ( Game, Cmd Msg )
    | PlayerWon ( Game, Cmd Msg )
    | PlayerLost ( Game, Cmd Msg )


isGameLost : Game -> Bool
isGameLost game =
    Party.isEveryoneDead game.party


isGameWon : Game -> Bool
isGameWon game =
    Meter.getCurrent game.enemy.health == 0


updateGame : EngineArgs -> Msg -> Game -> GameUpdate
updateGame engineArgs msg game =
    let
        noOp =
            ContinueGame ( game, Cmd.none )
    in
    case msg of
        NoOp ->
            noOp

        DeselectAction ->
            let
                newGame =
                    game
                        |> updateActions SelectionList.clearSelection
            in
            ContinueGame ( newGame, Cmd.none )

        SelectAction position ->
            SelectionList.select { liveInputs = [] } position game.actions
                |> Maybe.map
                    (\newActions ->
                        let
                            newGame =
                                { game | actions = newActions }
                        in
                        ContinueGame ( newGame, emitSound sounds.select )
                    )
                |> Maybe.withDefault noOp

        Input input ->
            let
                updateInputs : Action -> Selection -> Selection
                updateInputs action selection =
                    case action of
                        EnemyMove _ ->
                            selection

                        AllyMove _ _ move ->
                            let
                                nextInput : Maybe Input
                                nextInput =
                                    Utils.getNextInput move.recipe (List.reverse selection.liveInputs)

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
            SelectionList.mapSelection updateInputs game.actions
                |> Maybe.map
                    (\newActions ->
                        let
                            newGame =
                                { game | actions = newActions }
                        in
                        ContinueGame ( newGame, emitSound sounds.attack )
                    )
                |> Maybe.withDefault noOp

        Finish ->
            case SelectionList.getSelected game.actions of
                Nothing ->
                    -- Finishing when nothing is selected shouldn't normally happen
                    noOp

                Just ( action, selection ) ->
                    case action of
                        EnemyMove _ ->
                            Debug.todo "Implement enemy move"

                        AllyMove _ _ move ->
                            if Utils.isPatternComplete move.recipe (List.reverse selection.liveInputs) then
                                let
                                    newGame =
                                        game
                                            |> applyOnSuccess move
                                            |> updateActions SelectionList.deleteSelected
                                            |> updateEnemy (\enemy -> { enemy | spriteAnimation = Just <| Animation.create Animation.Shake })
                                in
                                ContinueGame ( newGame, emitSound sounds.attack )

                            else
                                -- Finish was called without a completed pattern for a selected ally
                                let
                                    newGame =
                                        game
                                            |> updateActions SelectionList.deleteSelected
                                in
                                ContinueGame ( newGame, emitSound sounds.select )

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

                updateHealthMeter : { a | health : Meter } -> { a | health : Meter }
                updateHealthMeter obj =
                    let
                        newMeter =
                            Meter.handleAnimationFrameDebounce delta obj.health
                    in
                    { obj | health = newMeter }

                handleEnemyAttack : Game -> Game
                handleEnemyAttack oldGame =
                    let
                        enemy =
                            oldGame.enemy

                        readyToAttack =
                            Meter.isFull enemy.energy
                    in
                    if readyToAttack then
                        let
                            drainEnergy : Enemy -> Enemy
                            drainEnergy oldEnemy =
                                { enemy | energy = Meter.drain oldEnemy.energy }
                        in
                        oldGame
                            |> updateHealth (Meter.subtract (toFloat enemy.stats.damage))
                            |> updateEnemy drainEnergy
                            |> updateEnemy (updateEnemyEnergy delta)

                    else
                        oldGame
                            |> updateEnemy (updateEnemyEnergy delta)

                updateActionAnimation : Action -> Action
                updateActionAnimation action =
                    case action of
                        EnemyMove timer ->
                            EnemyMove (ActionTimer.handleAnimationFrame delta timer)

                        AllyMove timer ally move ->
                            AllyMove (ActionTimer.handleAnimationFrame delta timer) ally move

                updateActionsAnimation : SelectionList Action Selection -> SelectionList Action Selection
                updateActionsAnimation list =
                    list
                        |> SelectionList.mapUnselected updateActionAnimation

                newGame : Game
                newGame =
                    game
                        |> updateActions updateActionsAnimation
                        |> updateParty (Party.handleAnimationFrame delta)
                        |> handleEnemyAttack
                        |> updateEnemy updateAnimation
                        |> updateEnemy updateHealthMeter
                        |> removeDoneActions
                        |> addNewAllyActions
            in
            if isGameLost newGame then
                PlayerLost ( newGame, Cmd.none )

            else if isGameWon newGame then
                PlayerWon ( newGame, Cmd.none )

            else
                ContinueGame ( newGame, Cmd.none )



-- SUBSCRIPTIONS


keyDecoder : EngineArgs -> Model -> Decode.Decoder Msg
keyDecoder engineArgs model =
    case model of
        GameInProgress game ->
            Decode.map (toUserInput engineArgs game) (Decode.field "key" Decode.string)

        _ ->
            Decode.succeed NoOp


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


getAvailableInputs : Game -> List Input
getAvailableInputs game =
    SelectionList.getSelected game.actions
        |> Maybe.map
            (\( action, _ ) ->
                case action of
                    AllyMove _ _ move ->
                        move.inputs

                    EnemyMove _ ->
                        []
            )
        |> Maybe.withDefault []


toUserInput : EngineArgs -> Game -> String -> Msg
toUserInput engineArgs game string =
    let
        result =
            tryAll
                [ matchGlobalAction string
                , matchSelectionAction game string
                ]
    in
    Maybe.withDefault NoOp result


findInputMatchingString : String -> List Input -> Maybe Msg
findInputMatchingString string inputs =
    let
        inputMatchesString : String -> Input -> Bool
        inputMatchesString iterString ( trigger, _ ) =
            iterString == String.fromChar trigger
    in
    inputs
        |> List.Extra.find (inputMatchesString string)
        |> Maybe.map Input


matchGlobalAction : String -> Maybe Msg
matchGlobalAction string =
    case string of
        "1" ->
            Just (SelectAction 0)

        "2" ->
            Just (SelectAction 1)

        "3" ->
            Just (SelectAction 2)

        "4" ->
            Just (SelectAction 3)

        "5" ->
            Just (SelectAction 4)

        "Escape" ->
            Just DeselectAction

        "q" ->
            Just DeselectAction

        "Enter" ->
            Just Finish

        _ ->
            Nothing


matchSelectionAction : Game -> String -> Maybe Msg
matchSelectionAction game string =
    game
        |> getAvailableInputs
        |> findInputMatchingString string


subscriptions : EngineArgs -> Model -> Sub Msg
subscriptions engineArgs model =
    case model of
        GameInProgress _ ->
            Sub.batch
                [ Browser.Events.onKeyUp (keyDecoder engineArgs model)
                , Browser.Events.onAnimationFrameDelta HandleAnimationFrame
                ]

        _ ->
            Sub.none



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


renderActionList : Game -> Html Msg
renderActionList game =
    let
        actions =
            game.actions

        actionContainer =
            "flex w-full h-12 items-center overflow-hidden relative"

        actionContent =
            "h-10 w-36 border border-black border-l-0 text-sm overflow-hidden flex justify-center items-center shadow rounded-r cursor-pointer absolute top-0 bottom-0 m-auto"

        renderActionNumber : Int -> Msg -> Html Msg
        renderActionNumber index handleClick =
            div
                [ class "h-full w-12 border-2 border-black border-l-0 flex justify-center items-center cursor-pointer bg-gray-100 z-10"
                , onClick handleClick
                ]
                [ div [] [ text <| String.fromInt (index + 1) ] ]

        renderUnselectedAction : Int -> Action -> Html Msg
        renderUnselectedAction index action =
            case action of
                EnemyMove timer ->
                    div [ class actionContainer, onClick (SelectAction index) ]
                        [ renderActionNumber index (SelectAction index)
                        , div [ class actionContent, style "left" "-50%" ]
                            []
                        ]

                AllyMove timer ally move ->
                    div [ class actionContainer, onClick (SelectAction index) ]
                        [ renderActionNumber index (SelectAction index)
                        , div [ class "relative h-full w-36" ]
                            [ div [ class actionContent, style "left" (String.fromFloat (ActionTimer.getLeft timer) ++ "%") ]
                                [ img [ src ally.stats.avatarUrl ] [] ]
                            ]
                        ]

        renderSelectedAction : Int -> ( Action, Selection ) -> Html Msg
        renderSelectedAction index ( action, selection ) =
            case action of
                EnemyMove _ ->
                    div [ class actionContainer, onClick DeselectAction ]
                        [ renderActionNumber index DeselectAction
                        , div [ class actionContent, class "border-dashed" ]
                            []
                        ]

                AllyMove _ ally move ->
                    div [ class actionContainer, onClick DeselectAction ]
                        [ renderActionNumber index DeselectAction
                        , div [ class "relative h-full w-36" ]
                            [ div [ class actionContent, class "border-dashed" ]
                                [ img [ src ally.stats.avatarUrl ] [] ]
                            ]
                        ]

        renderNothing : Int -> Html Msg
        renderNothing index =
            div [ class actionContainer ]
                [ renderActionNumber index NoOp
                ]
    in
    div [ class "border border-dashed border-blue-500 h-full w-64" ]
        [ div [ class "w-full h-full flex flex-col space-y-4 mt-4" ]
            (actions
                |> SelectionList.indexedMap renderUnselectedAction renderSelectedAction renderNothing
            )
        ]


renderTop : Game -> Html Msg
renderTop game =
    let
        allyStatsContainer =
            "w-8"

        renderAliveAlly : Ally -> Html Msg
        renderAliveAlly ally =
            let
                { stats, energy } =
                    ally

                { battleUrl } =
                    stats

                allyImage =
                    div [ class "relative" ]
                        [ img [ class "w-24 h-24", class (Animation.classForAnimation ally.spriteAnimation), src battleUrl ] []
                        ]
            in
            div [ class "flex-col space-y-2" ]
                [ div [ class "flex items-end" ]
                    [ allyImage ]
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
                (game.party
                    |> Party.map
                        (\allySpot ->
                            div [ class "w-full h-1/3 flex items-center" ]
                                [ div
                                    [ class "ml-4" ]
                                    [ case allySpot of
                                        Party.AliveAlly ally ->
                                            renderAliveAlly ally

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
        [ renderActionList game
        , renderAllies
        , renderEnemy game.enemy
        ]


renderBottom : Game -> Html Msg
renderBottom game =
    case SelectionList.getSelected game.actions of
        Nothing ->
            div [] []

        Just ( AllyMove _ ally move, selection ) ->
            renderAllyBottom ally move selection

        Just ( EnemyMove _, _ ) ->
            div [] []


renderAllyBottom : Ally -> Ally.Move -> Selection -> Html Msg
renderAllyBottom ally move selection =
    let
        renderPortrait : Html Msg
        renderPortrait =
            div [ class "overflow-hidden w-48 h-48 relative" ]
                [ div [ class "bg-blue-200 border-4 border-gray-900" ] [ img [ src ally.stats.avatarUrl, class "bg-blue-200" ] [] ]
                ]

        renderPrompt : Html Msg
        renderPrompt =
            div [ class "w-64 h-48" ]
                [ div [ class "italic" ] [ text move.prompt ]
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

        renderFinish : Html Msg
        renderFinish =
            button [ class inputContainer, onClick Finish ]
                [ div [ class inputTrigger ] [ text "Enter" ]
                , div [ class inputLabel ] [ text "Finish" ]
                ]

        renderMove : Html Msg
        renderMove =
            div [ class "w-96 h-48 " ]
                [ div [ class "h-full w-full flex-col" ]
                    [ div [ class "w-96 h-40" ]
                        [ move.inputs
                            |> List.map renderInput
                            |> div [ class "flex space-x-2 flex-wrap" ]
                        ]
                    , div [ class "w-96 h-8" ] [ renderFinish ]
                    ]
                ]

        renderLiveInput : Input -> Html Msg
        renderLiveInput ( _, label ) =
            div [] [ text label ]

        renderInputs : Html Msg
        renderInputs =
            div [ class "flex-grow h-48 border border-gray-900" ]
                [ div [ class "flex-col" ] (List.map renderLiveInput selection.liveInputs)
                ]
    in
    div [ class "w-full h-full border-gray-500 border-4 bg-gray-400 flex items-center p-2 space-x-2" ]
        [ renderPortrait
        , renderPrompt
        , renderMove
        , renderInputs
        ]


view : EngineArgs -> Model -> Html Msg
view engineArgs model =
    let
        renderGame : Game -> Html Msg
        renderGame game =
            div [ class "bg-gray-900 w-screen h-screen flex items-center justify-center" ]
                [ div
                    [ class "rounded border-gray-100 border-4"
                    , style "width" "64rem" -- i.e. w-256
                    , style "height" "42rem" -- i.e. h-168
                    ]
                    [ div [ class "w-full h-2/3" ] [ renderTop game ]
                    , div [ class "w-full h-1/3" ] [ renderBottom game ]
                    ]
                ]
    in
    case model of
        GameInProgress game ->
            renderGame game

        GameLost game ->
            div []
                [ div [ class "fixed w-screen h-screen bg-gray-700 bg-opacity-75 z-10" ]
                    [ div [ class "absolute w-full flex justify-center top-20" ]
                        [ div [ class "p-12 text-2xl font-bold border-4 border-gray-900 bg-gray-500 text-gray-100" ] [ text "Game Lost" ] ]
                    ]
                , renderGame game
                ]

        GameWon game ->
            div []
                [ div [ class "fixed w-screen h-screen bg-gray-700 bg-opacity-75 z-10" ]
                    [ div [ class "absolute w-full flex justify-center top-20" ]
                        [ div [ class "p-12 text-2xl font-bold border-4 border-gray-900 bg-green-500 text-gray-900" ] [ text "Game Won" ] ]
                    ]
                , renderGame game
                ]
