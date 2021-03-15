module Engine exposing (Instance, create)

import Browser
import Browser.Events
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode


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


type alias EngineArgAlly =
    { avatarUrl : String
    , battleUrl : String
    }


type alias EngineArgs =
    { title : String
    , allyOne : EngineArgAlly
    , allyTwo : EngineArgAlly
    , allyThree : EngineArgAlly
    }


create : EngineArgs -> Instance
create engineArgs =
    Browser.element { init = init, update = update, view = view engineArgs, subscriptions = subscriptions }



-- MODEL


type Position
    = First
    | Second
    | Third


type alias Model =
    { allySelection : Maybe Position }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { allySelection = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | SetAllySelection (Maybe Position)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetAllySelection position ->
            ( { model | allySelection = position }, Cmd.none )



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toUserInput (Decode.field "key" Decode.string)


toUserInput : String -> Msg
toUserInput string =
    case string of
        "1" ->
            SetAllySelection (Just First)

        "2" ->
            SetAllySelection (Just Second)

        "3" ->
            SetAllySelection (Just Third)

        "Escape" ->
            SetAllySelection Nothing

        "q" ->
            SetAllySelection Nothing

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyUp keyDecoder ]



-- VIEW


renderTop : EngineArgs -> Model -> Html Msg
renderTop engineArgs model =
    let
        renderAlly : Bool -> String -> Msg -> Html Msg
        renderAlly isSelected imageUrl selectionMsg =
            div [ class "relative" ]
                [ img [ class "w-24 h-24", src imageUrl, onClick selectionMsg ] []
                , if isSelected then
                    -- img [ class "absolute inline-block -bottom-6 ml-24 w-4 h-4", src selectionArrow ] []
                    img [ class "absolute inline-block w-24 h-24 top-0 left-0", src battleSelection ] []

                  else
                    div [] []
                ]

        renderAllies =
            div [ class "border border-dashed h-full w-96 flex-col" ]
                [ div [ class "w-full h-1/3 flex items-center" ]
                    [ div
                        [ class "ml-4" ]
                        [ renderAlly (model.allySelection == Just First) engineArgs.allyOne.battleUrl (SetAllySelection (Just First))
                        ]
                    ]
                , div [ class "w-full h-1/3 flex items-center" ]
                    [ div
                        [ class "ml-32" ]
                        [ renderAlly (model.allySelection == Just Second) engineArgs.allyTwo.battleUrl (SetAllySelection (Just Second)) ]
                    ]
                , div [ class "w-full h-1/3 flex items-center" ]
                    [ div
                        [ class "ml-4" ]
                        [ renderAlly (model.allySelection == Just Third) engineArgs.allyThree.battleUrl (SetAllySelection (Just Third))
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
            case position of
                First ->
                    engineArgs.allyOne.avatarUrl

                Second ->
                    engineArgs.allyTwo.avatarUrl

                Third ->
                    engineArgs.allyThree.avatarUrl

        renderPortrait : String -> Html Msg
        renderPortrait url =
            div [ class "overflow-hidden w-48 h-48 relative bg-blue-200 border-4 border-gray-900" ]
                [ img [ src url ] [] ]
    in
    div [ class "w-full h-full border-gray-500 border-4 bg-gray-400 flex items-center p-2" ]
        [ case model.allySelection of
            Nothing ->
                div [] []

            Just position ->
                renderPortrait <| avatarUrlForPosition position
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
