port module Engine exposing (EngineArgEnemy, Instance, create)

import Action exposing (Action, ActionModel, ActionType)
import ActionTimer exposing (ActionTimer)
import Ally exposing (Ally)
import Animation exposing (Animation)
import Browser
import Browser.Events
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Input exposing (Input)
import Json.Decode as Decode
import List.Extra
import Meter exposing (Meter)
import Party exposing (Party)
import Random
import SelectionList exposing (SelectionList)
import Task
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


type alias EngineArgEnemy =
    { battleUrl : String
    , maxHealth : Int
    , maxEnergy : Int
    , moves : List ActionType
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


type alias ActionList =
    SelectionList Action ActionModel


type alias Enemy =
    { stats : EngineArgEnemy
    , health : Meter
    , energy : Meter
    , spriteAnimation : Maybe Animation
    }


type alias Game =
    { nonce : Int
    , seed : Random.Seed
    , party : Party
    , enemy : Enemy
    , actions : ActionList
    , health : Meter
    }


type GameStatus
    = GameInProgress Game
    | GameWon Game
    | GameLost Game


type alias Model =
    { keysDown : List Input
    , game : GameStatus
    }


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

        initialActions : ActionList
        initialActions =
            SelectionList.create 5

        initialGameStatus : GameStatus
        initialGameStatus =
            GameInProgress
                { nonce = 0
                , seed = Random.initialSeed 0
                , party = initialParty
                , enemy = initialEnemy
                , actions = initialActions
                , health = Meter.create 100
                }
    in
    ( { keysDown = [], game = initialGameStatus }
    , Cmd.none
    )



-- UPDATE


port emitSound : String -> Cmd msg


type Msg
    = NoOp
    | HandleKeyDown String
    | HandleKeyUp String
    | SelectAction Int
    | DeselectAction
    | Input Input
    | HandleKnightInput Action.KnightInput
    | Finish
    | HandleAnimationFrame Float


updateParty : (Party -> Party) -> Game -> Game
updateParty updateFn game =
    { game | party = updateFn game.party }


updateActions : (ActionList -> ActionList) -> Game -> Game
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


incrementNonce : Game -> Game
incrementNonce game =
    { game | nonce = game.nonce + 1 }


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
                            randomActionType =
                                Random.uniform first rest

                            ( actionType, newSeed ) =
                                Random.step randomActionType accumGame.seed

                            action : Action
                            action =
                                Action.create accumGame.nonce actionType
                        in
                        accumGame
                            |> incrementNonce
                            |> addAction action
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


removeExpiredActions : Game -> Game
removeExpiredActions game =
    game
        |> updateActions (SelectionList.filterUnselected (Action.isExpired >> not))


applyOnSuccess : Action -> Game -> Game
applyOnSuccess action game =
    let
        effects =
            action
                |> Action.getStats
                |> .onSuccess

        applyEffect : Action.Effect -> Game -> Game
        applyEffect effect g =
            case effect of
                Action.Damage amount ->
                    dealDamageToEnemy amount g
    in
    List.foldl applyEffect game effects


update : EngineArgs -> Msg -> Model -> ( Model, Cmd Msg )
update engineArgs msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleKeyDown keyCode ->
            keyCode
                |> Input.matchStringToInput
                |> Maybe.map (\input -> ( { model | keysDown = input :: model.keysDown }, Cmd.none ))
                |> Maybe.withDefault noOp

        HandleKeyUp keyCode ->
            case Input.matchStringToInput (Debug.log "up:" keyCode) of
                Just input ->
                    let
                        maybeNextMsg =
                            matchInputToMsg input
                    in
                    case maybeNextMsg of
                        Just nextMsg ->
                            -- TODO: model returned should have the inputs removed from keysDown
                            ( model, Task.perform (\() -> nextMsg) (Task.succeed ()) )

                        Nothing ->
                            noOp

                Nothing ->
                    noOp

        -- keyCode
        --     |> Input.matchStringToInput
        --     |> Maybe.map (\input ->
        --         input
        --             |> matchInputToMsg
        --             |>
        --         let
        --             nextMsg = matchInputToMsg input
        --         in
        --         n
        --         noOp)
        --     |> Maybe.withDefault noOp
        _ ->
            case model.game of
                GameInProgress game ->
                    case updateGame engineArgs msg game of
                        ContinueGame ( newGame, commands ) ->
                            ( { model | game = GameInProgress newGame }, commands )

                        PlayerWon ( newGame, commands ) ->
                            ( { model | game = GameWon newGame }, commands )

                        PlayerLost ( newGame, commands ) ->
                            ( { model | game = GameLost newGame }, commands )

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


mapKnightModel : (List Action.KnightInput -> List Action.KnightInput) -> Game -> Game
mapKnightModel fn game =
    let
        newActions : ActionList
        newActions =
            game.actions
                |> SelectionList.mapSelection
                    (\actionModel ->
                        case actionModel of
                            Action.KnightModel list ->
                                Action.KnightModel (fn list)

                            _ ->
                                actionModel
                    )
                |> Maybe.withDefault game.actions
    in
    { game | actions = newActions }


addShake : { a | spriteAnimation : Maybe Animation } -> { a | spriteAnimation : Maybe Animation }
addShake obj =
    { obj | spriteAnimation = Just <| Animation.create Animation.Shake }


updateGame : EngineArgs -> Msg -> Game -> GameUpdate
updateGame engineArgs msg game =
    let
        noOp =
            ContinueGame ( game, Cmd.none )
    in
    case msg of
        NoOp ->
            -- TODO : refactor update function so dont have to have this here
            noOp

        HandleKeyDown _ ->
            noOp

        HandleKeyUp _ ->
            noOp

        DeselectAction ->
            let
                newGame =
                    game
                        |> updateActions SelectionList.clearSelection
            in
            ContinueGame ( newGame, Cmd.none )

        SelectAction position ->
            SelectionList.select position Action.getModel game.actions
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
            -- let
            --     updateInputs : Action -> Selection -> Selection
            --     updateInputs action selection =
            --         case action of
            --             EnemyMove _ ->
            --                 selection
            --             AllyMove _ _ move ->
            --                 let
            --                     nextInput : Maybe Input
            --                     nextInput =
            --                         Utils.getNextInput move.recipe (List.reverse selection.liveInputs)
            --                     inputMatches =
            --                         nextInput
            --                             |> Maybe.map ((==) input)
            --                             |> Maybe.withDefault False
            --                 in
            --                 if inputMatches then
            --                     addInputToSelection input selection
            --                 else
            --                     selection
            -- in
            -- SelectionList.mapSelection updateInputs game.actions
            --     |> Maybe.map
            --         (\newActions ->
            --             let
            --                 newGame =
            --                     { game | actions = newActions }
            --             in
            --             ContinueGame ( newGame, emitSound sounds.attack )
            --         )
            --     |> Maybe.withDefault noOp
            Debug.todo "implement input"

        HandleKnightInput input ->
            let
                newGame : Game
                newGame =
                    game
                        |> mapKnightModel (Action.handleKnightInput input)
            in
            ContinueGame ( newGame, Cmd.none )

        Finish ->
            case SelectionList.getSelected game.actions of
                Nothing ->
                    -- Finishing when nothing is selected shouldn't normally happen
                    noOp

                Just ( action, Action.KnightModel inputs ) ->
                    let
                        effects =
                            Action.getStats action
                                |> .onSuccess

                        applyEffect : Action.Effect -> Game -> Game
                        applyEffect effect g =
                            case effect of
                                Action.Damage amount ->
                                    dealDamageToEnemy amount g

                        newGame : Game
                        newGame =
                            List.foldl applyEffect game effects
                                |> updateActions SelectionList.deleteSelected
                    in
                    ContinueGame ( newGame, emitSound sounds.select )

                _ ->
                    Debug.todo "Implement other moves"

        -- AllyMove _ _ move ->
        --     if Utils.isPatternComplete move.recipe (List.reverse selection.liveInputs) then
        --         let
        --             newGame =
        --                 game
        --                     |> applyOnSuccess move
        --                     |> updateActions SelectionList.deleteSelected
        --                     |> updateEnemy (\enemy -> { enemy | spriteAnimation = Just <| Animation.create Animation.Shake })
        --         in
        --         ContinueGame ( newGame, emitSound sounds.attack )
        --     else
        --         -- Finish was called without a completed pattern for a selected ally
        --         let
        --             newGame =
        --                 game
        --                     |> updateActions SelectionList.deleteSelected
        --         in
        --         ContinueGame ( newGame, emitSound sounds.select )
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
                            |> updateEnemy addShake

                    else
                        oldGame
                            |> updateEnemy (updateEnemyEnergy delta)

                updateActionAnimation : Action -> Action
                updateActionAnimation =
                    Action.mapTimer (ActionTimer.handleAnimationFrame delta)

                updateActionsAnimation : ActionList -> ActionList
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
                        |> removeExpiredActions
                        |> addNewAllyActions
            in
            if isGameLost newGame then
                PlayerLost ( newGame, Cmd.none )

            else if isGameWon newGame then
                PlayerWon ( newGame, Cmd.none )

            else
                ContinueGame ( newGame, Cmd.none )



-- SUBSCRIPTIONS


keyUpDecoder : EngineArgs -> Model -> Decode.Decoder Msg
keyUpDecoder engineArgs model =
    case model.game of
        GameInProgress game ->
            Decode.map HandleKeyUp (Decode.field "key" Decode.string)

        _ ->
            Decode.succeed NoOp


keyDownDecoder : EngineArgs -> Model -> Decode.Decoder Msg
keyDownDecoder engineArgs model =
    Decode.map HandleKeyDown (Decode.field "key" Decode.string)


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


toUserInput : EngineArgs -> Game -> String -> Msg
toUserInput engineArgs game string =
    Debug.log "string" string
        |> Input.matchStringToInput
        |> Maybe.andThen matchInputToMsg
        |> Maybe.withDefault NoOp


matchInputToMsg : Input -> Maybe Msg
matchInputToMsg input =
    -- As written matchInput is a bit dumb. It doesn't consider the context of the game when matching.
    -- We might want to e.g. map Input.Cancel to Finish or CloseDialog if a dialog is open in the future.
    -- That logic could live in this function.
    case Debug.log "input" input of
        Input.SelectOne ->
            Just (SelectAction 0)

        Input.SelectTwo ->
            Just (SelectAction 1)

        Input.SelectThree ->
            Just (SelectAction 2)

        Input.SelectFour ->
            Just (SelectAction 3)

        Input.SelectFive ->
            Just (SelectAction 4)

        Input.Cancel ->
            Just DeselectAction

        Input.Finish ->
            Just Finish

        Input.Slash ->
            Just (HandleKnightInput Action.Slash)

        Input.Kick ->
            Just (HandleKnightInput Action.Kick)

        Input.Wait ->
            Just (HandleKnightInput Action.Wait)

        Input.Thrust ->
            Just (HandleKnightInput Action.Thrust)


subscriptions : EngineArgs -> Model -> Sub Msg
subscriptions engineArgs model =
    case model.game of
        GameInProgress _ ->
            Sub.batch
                [ Browser.Events.onKeyUp (keyUpDecoder engineArgs model)
                , Browser.Events.onKeyDown (keyDownDecoder engineArgs model)
                , Browser.Events.onAnimationFrameDelta HandleAnimationFrame
                ]

        _ ->
            Sub.none



-- VIEW


inputContainer : String
inputContainer =
    "flex items-center h-8 overflow-hidden border-2 border-gray-900 cursor-pointer shadow"


inputTrigger : String
inputTrigger =
    "py-1 px-2 bg-gray-900 text-gray-100 active"


inputLabel : String
inputLabel =
    "py-1 px-2 bg-blue-500 active:bg-blue-300"


inputLabelActive : String
inputLabelActive =
    "py-1 px-2 bg-blue-300"


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
            let
                timeLeft =
                    action
                        |> Action.getTimer
                        |> ActionTimer.getLeft

                { avatarUrl, bgColor } =
                    Action.getStats action
            in
            div [ class actionContainer, onClick (SelectAction index) ]
                [ renderActionNumber index (SelectAction index)
                , div [ class "relative h-full w-36" ]
                    [ div [ class actionContent, class bgColor, style "left" (String.fromFloat timeLeft ++ "%") ]
                        [ img [ src avatarUrl ] [] ]
                    ]
                ]

        renderSelectedAction : Int -> ( Action, ActionModel ) -> Html Msg
        renderSelectedAction index ( action, actionModel ) =
            let
                { avatarUrl, bgColor } =
                    Action.getStats action
            in
            div [ class actionContainer, onClick DeselectAction ]
                [ renderActionNumber index DeselectAction
                , div [ class "relative h-full w-36" ]
                    [ div [ class actionContent, class bgColor, class "border-dashed" ]
                        [ img [ src avatarUrl ] [] ]
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
    SelectionList.getSelected game.actions
        |> Maybe.map
            (\( action, actionModel ) ->
                case ( Action.getActionType action, actionModel ) of
                    ( Action.KnightAttack, Action.KnightModel inputs ) ->
                        renderKnightBottom action inputs

                    ( Action.ThiefAttack, Action.ThiefModel ) ->
                        div [] []

                    ( Action.PriestAttack, Action.PriestModel ) ->
                        div [] []

                    ( Action.EnemyAttack, Action.EnemyModel ) ->
                        div [] []

                    _ ->
                        div [] []
             -- ThiefSelection ->
             --     div [ class "text-gray-100" ] [ text "Thief Selection" ]
             -- PriestSelection ->
             --     div [ class "text-gray-100" ] [ text "Priest Selection" ]
             -- EnemySelection ->
             --     div [ class "text-gray-100" ] [ text "Enemy Selection" ]
            )
        |> Maybe.withDefault (div [ class "text-gray-100" ] [ text "Nothing Selected" ])


renderKnightBottom : Action -> List Action.KnightInput -> Html Msg
renderKnightBottom action knightModel =
    let
        stats =
            Action.getStats action

        renderPortrait : Html Msg
        renderPortrait =
            div [ class "overflow-hidden w-48 h-48 relative" ]
                [ div [ class "border-4 border-gray-900" ] [ img [ src stats.avatarUrl, class stats.bgColor ] [] ]
                ]

        renderPrompt : Html Msg
        renderPrompt =
            div [ class "h-12 flex items-center justify-center" ]
                [ div [ class "italic" ] [ text "For this opponent...something special. A kick, a slash, then wait, then thrust!" ]
                ]

        renderInput : Bool -> Action.KnightInput -> Html Msg
        renderInput isPressed input =
            let
                { keyCode, label } =
                    Action.knightInputStats input
            in
            button
                [ class inputContainer
                , onClick (HandleKnightInput input)
                ]
                [ div [ class inputTrigger ] [ text <| String.fromChar keyCode ]
                , div
                    [ class <|
                        if isPressed then
                            inputLabelActive

                        else
                            inputLabel
                    ]
                    [ text label ]
                ]

        renderFinishButton : Html Msg
        renderFinishButton =
            div [ onClick Finish ] [ text "Finish" ]

        renderMoves : Html Msg
        renderMoves =
            div [ class "w-96 h-20 flex items-center space-x-1" ]
                (List.concat
                    [ [ Action.Slash, Action.Kick, Action.Wait, Action.Thrust ]
                        |> List.map (renderInput True)
                    , [ renderFinishButton ]
                    ]
                )

        renderInputs : Html Msg
        renderInputs =
            div [ class "flex space-x-2" ]
                (knightModel
                    |> List.map (renderInput True)
                )
    in
    div [ class "w-full h-full border-gray-500 border-4 bg-gray-400 flex items-center p-2 space-x-2" ]
        [ renderPortrait
        , div [ class "flex-grow flex flex-col space-y-2 items-stretch justify-start h-full" ]
            [ renderPrompt, renderMoves, renderInputs ]
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
    case model.game of
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
