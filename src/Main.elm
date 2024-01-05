module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Random
import RingWorld as World exposing (World)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



-- ENTITY


type Entity
    = Square Float
    | Triangle ( Float, Float )
    | Spawner ( Float, Float ) Entity


viewEntity : Entity -> Svg msg
viewEntity entity =
    case entity of
        Square _ ->
            Svg.rect
                [ Svg.Attributes.width "50"
                , Svg.Attributes.height "50"
                , Svg.Attributes.x "-25"
                , Svg.Attributes.y "-25"
                , Svg.Attributes.fill "hsl(120, 85%, 75%)"
                , Svg.Attributes.stroke "beige"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinejoin "round"
                ]
                []

        Triangle ( growth, maxGrowth ) ->
            Svg.polygon
                [ Svg.Attributes.points "0,-35 30.4,17.5 -30.4,17.5"
                , Svg.Attributes.fill "hsl(320, 85%, 75%)"
                , Svg.Attributes.stroke "beige"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinejoin "round"
                , Svg.Attributes.transform ("translate(0, 10) scale(" ++ String.fromFloat (growth / maxGrowth) ++ ")")
                ]
                []

        Spawner _ spawnEntity ->
            Svg.g [ Svg.Attributes.style "opacity: 0.2" ] [ viewEntity spawnEntity ]



-- LOGICSYSTEM


type LogicSystem
    = Movement
    | Spawn
    | Growth


runLogicSystem : Float -> Int -> Float -> Entity -> LogicSystem -> ( ( Float, Entity ), List (World.Cmd Entity) )
runLogicSystem dt id position entity system =
    case system of
        Movement ->
            case entity of
                Square velocity ->
                    ( ( position + (dt * velocity), entity )
                    , []
                    )

                _ ->
                    ( ( position, entity )
                    , []
                    )

        Spawn ->
            case entity of
                Spawner ( cd, maxCd ) spawnEntity ->
                    if cd - dt <= 0 then
                        ( ( position, Spawner ( maxCd, maxCd ) spawnEntity )
                        , [ World.addEntityRandomPosCmd (Random.float (position - 200) (position + 200)) spawnEntity ]
                        )

                    else
                        ( ( position, Spawner ( max 0 (cd - dt), maxCd ) spawnEntity )
                        , []
                        )

                _ ->
                    ( ( position, entity )
                    , []
                    )

        Growth ->
            case entity of
                Triangle ( growth, maxGrowth ) ->
                    if growth + dt >= maxGrowth then
                        ( ( position, entity )
                        , [ World.removeEntityCmd id ]
                        )

                    else
                        ( ( position, Triangle ( growth + dt |> min maxGrowth, maxGrowth ) )
                        , []
                        )

                _ ->
                    ( ( position, entity )
                    , []
                    )



-- RENDERSYSTEM


type RenderSystem
    = Debug
    | Shape


runRenderSystem : Float -> Entity -> RenderSystem -> Svg msg
runRenderSystem position entity system =
    case system of
        Debug ->
            Svg.g []
                [ Svg.text_
                    [ Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.fill "beige"
                    , Svg.Attributes.transform "translate(0, 50)"
                    ]
                    [ Svg.text ("pos: " ++ prettyFloat position) ]
                ]

        Shape ->
            viewEntity entity



-- MODEL


type alias Model =
    World Entity LogicSystem RenderSystem


init : () -> ( Model, Cmd Msg )
init _ =
    ( World.empty
        -- |> World.addRenderSystem Debug
        |> World.addRenderSystem Shape
        |> World.addLogicSystem Movement
        |> World.addLogicSystem Spawn
        |> World.addLogicSystem Growth
        |> World.addEntity 0 (Square 0.07)
        |> World.addEntity 200 (Square -0.05)
        |> World.addEntity 800 (Square 0.1)
        |> World.addEntity 500 (Spawner ( 1000, 1000 ) (Triangle ( 0, 5000 )))
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedMoveCamera Float
    | SetCameraPosition Float
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model |> World.runLogicSystems (runLogicSystem dt), Cmd.none )

        ClickedMoveCamera delta ->
            ( model |> World.moveCamera delta, Cmd.none )

        SetCameraPosition pos ->
            ( model |> World.setCameraPos pos, Cmd.none )



-- VIEW


prettyFloat : Float -> String
prettyFloat n =
    case n |> String.fromFloat |> String.split "." of
        [ x ] ->
            x

        [ x, y ] ->
            x ++ "." ++ String.left 1 y

        _ ->
            "err"


view : Model -> Html Msg
view model =
    main_ []
        [ Html.div
            [ Html.Attributes.style "padding" "1rem 5rem"
            ]
            [ Html.p [ Html.Attributes.style "text-align" "center" ] [ Html.text ("Camera pos: " ++ prettyFloat (World.getCameraPosition model)) ]
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.max (World.mapSize model |> String.fromFloat)
                , Html.Attributes.step "0.1"
                , Html.Attributes.value (World.getCameraPosition model |> String.fromFloat)
                , Html.Events.onInput (String.toFloat >> Maybe.withDefault (World.getCameraPosition model) >> SetCameraPosition)
                , Html.Attributes.style "width" "100%"
                ]
                []
            ]
        , Svg.svg
            [ Svg.Attributes.class "svg-map"
            , Svg.Attributes.width "1000"
            , Svg.Attributes.height "500"
            , Svg.Attributes.viewBox "-500 -250 1000 500"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            , Svg.Events.on "wheel" (wheelDecoder ClickedMoveCamera)
            ]
            [ World.runRenderSystems 600 runRenderSystem model
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- DECODERS


wheelDecoder : (Float -> msg) -> Decoder msg
wheelDecoder wheelMovedMsg =
    let
        largest n1 n2 =
            if abs n1 > abs n2 then
                n1

            else
                n2
    in
    Decode.map2 (\x y -> wheelMovedMsg (largest x y))
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
