module Main exposing (Model, Msg, main, test)

import Browser
import Browser.Events
import Dict
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


test : Bool
test =
    variantEq (Square 100) (Square 50)


variantEq : Entity -> Entity -> Bool
variantEq e1 e2 =
    case e1 of
        Square _ ->
            case e2 of
                Square _ ->
                    True

                _ ->
                    False

        Triangle _ ->
            case e2 of
                Triangle _ ->
                    True

                _ ->
                    False

        Spawner _ _ ->
            case e2 of
                Spawner _ _ ->
                    True

                _ ->
                    False


viewEntity : Int -> Entity -> Svg Msg
viewEntity id entity =
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
                , Svg.Events.onClick (ClickedEntity id entity)
                ]
                []

        Spawner _ _ ->
            Svg.g [] []



-- LOGICSYSTEM


type LogicSystem
    = Movement
    | Spawn
    | Growth


moveSystem : Float -> Int -> Float -> Entity -> ( Float, Entity )
moveSystem dt _ position entity =
    case entity of
        Square velocity ->
            ( position + (dt * velocity), entity )

        _ ->
            ( position, entity )


spawnTimer : Float -> Int -> Float -> Entity -> ( Float, Entity )
spawnTimer dt _ position entity =
    case entity of
        Spawner ( cd, maxCd ) spawnEntity ->
            ( position, Spawner ( max 0 (cd - dt), maxCd ) spawnEntity )

        _ ->
            ( position, entity )


growthTimer : Float -> Int -> Float -> Entity -> ( Float, Entity )
growthTimer dt _ position entity =
    case entity of
        Triangle ( growth, maxGrowth ) ->
            if growth + dt >= maxGrowth then
                ( position, entity )

            else
                ( position, Triangle ( growth + dt |> min maxGrowth, maxGrowth ) )

        _ ->
            ( position, entity )


runLogicSystem : Float -> LogicSystem -> World Entity LogicSystem RenderSystem -> World Entity LogicSystem RenderSystem
runLogicSystem dt system world =
    case system of
        Movement ->
            World.mapEntities (moveSystem dt) world

        Spawn ->
            let
                isReady w _ pos entity =
                    case entity of
                        Spawner ( cd, _ ) spawnEntity ->
                            let
                                inRange =
                                    World.getEntitiesRange pos 200 w
                                        |> Dict.filter (\_ ( _, data ) -> variantEq data spawnEntity)
                                        |> Dict.size
                            in
                            cd <= 0 && inRange < 10

                        _ ->
                            False

                readyEntities w =
                    World.getFilteredEntities (isReady w) w
                        |> Dict.toList
                        |> List.map Tuple.second

                resetSpawnTimer _ pos entity =
                    case entity of
                        Spawner ( cd, maxCd ) spawnEntity ->
                            if cd <= 0 then
                                ( pos, Spawner ( maxCd, maxCd ) spawnEntity )

                            else
                                ( pos, entity )

                        _ ->
                            ( pos, entity )

                applySpawner : ( Float, Entity ) -> World Entity LogicSystem RenderSystem -> World Entity LogicSystem RenderSystem
                applySpawner ( pos, entity ) w =
                    case entity of
                        Spawner _ spawnEntity ->
                            World.addEntityRandomPos (Random.float (pos - 200) (pos + 200)) spawnEntity w

                        _ ->
                            w
            in
            World.mapEntities (spawnTimer dt) world
                |> (\w -> List.foldl applySpawner w (readyEntities w))
                |> World.mapEntities resetSpawnTimer

        Growth ->
            World.mapEntities (growthTimer dt) world



-- RENDERSYSTEM


type RenderSystem
    = Debug
    | Shape


runRenderSystem : Int -> Float -> Entity -> RenderSystem -> Svg Msg
runRenderSystem id position entity system =
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
            viewEntity id entity



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
        |> World.addEntity 500 (Triangle ( 0, 2000 ))
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedMoveCamera Float
    | SetCameraPosition Float
    | Tick Float
    | ClickedEntity Int Entity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model |> World.runLogicSystems (runLogicSystem dt), Cmd.none )

        ClickedMoveCamera delta ->
            ( model |> World.moveCamera delta, Cmd.none )

        SetCameraPosition pos ->
            ( model |> World.setCameraPos pos, Cmd.none )

        ClickedEntity id entity ->
            case entity of
                Triangle _ ->
                    ( model |> World.removeEntity id, Cmd.none )

                _ ->
                    ( model, Cmd.none )



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
