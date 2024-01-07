module Main exposing (Model, Msg, main)

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
    = Minion Float Float
    | Triangle ( Float, Float )
    | Spawner ( Float, Float ) Entity
    | Player


variantEq : Entity -> Entity -> Bool
variantEq e1 e2 =
    case e1 of
        Minion _ _ ->
            case e2 of
                Minion _ _ ->
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

        Player ->
            case e2 of
                Player ->
                    True

                _ ->
                    False


isReadyTriangle : Entity -> Bool
isReadyTriangle entity =
    case entity of
        Triangle ( growth, maxGrowth ) ->
            growth >= maxGrowth

        _ ->
            False


viewEntity : Int -> Entity -> Svg Msg
viewEntity id entity =
    case entity of
        Minion _ _ ->
            Svg.circle
                [ Svg.Attributes.r "10"
                , Svg.Attributes.cy "-35"
                , Svg.Attributes.fill "hsl(220, 85%, 75%)"
                , Svg.Attributes.stroke "beige"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.class "minion"
                , Svg.Attributes.style ("animation-delay: " ++ String.fromInt (id * 200) ++ "ms")
                ]
                []

        Triangle ( growth, maxGrowth ) ->
            Svg.polygon
                [ Svg.Attributes.points "0,-75 30.4,17.5 -30.4,17.5"
                , Svg.Attributes.fill "hsl(320, 85%, 75%)"
                , Svg.Attributes.stroke "beige"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinejoin "round"
                , Svg.Attributes.transform ("scale(" ++ String.fromFloat ((growth / maxGrowth) * 0.5) ++ " " ++ String.fromFloat (growth / maxGrowth) ++ ")")
                , Svg.Events.onClick (ClickedEntity id entity)
                , Svg.Attributes.class "triangle"
                ]
                []

        Spawner _ _ ->
            Svg.g [] []

        Player ->
            Svg.rect
                [ Svg.Attributes.width "30"
                , Svg.Attributes.height "30"
                , Svg.Attributes.x "-15"
                , Svg.Attributes.fill "hsl(120, 85%, 75%)"
                , Svg.Attributes.stroke "beige"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinejoin "round"
                , Svg.Attributes.class "player"
                ]
                []



-- LOGICSYSTEM


type LogicSystem
    = Spawn
    | Time
    | AI


entitiesInRange : Float -> Entity -> World Entity b c -> Dict.Dict Int ( Float, Entity )
entitiesInRange pos entity world =
    World.getEntitiesRange pos 200 world
        |> Dict.filter (\_ ( _, data ) -> variantEq data entity)


timeSystem : Float -> Int -> Float -> Entity -> ( Float, Entity )
timeSystem dt _ position entity =
    case entity of
        Minion velocity acceleration ->
            let
                newVelocity =
                    (velocity + (acceleration * dt)) * 0.95

                newPosition =
                    position + (newVelocity * dt)
            in
            ( newPosition, Minion newVelocity 0 )

        Spawner ( cd, maxCd ) spawnEntity ->
            ( position, Spawner ( max 0 (cd - dt), maxCd ) spawnEntity )

        Triangle ( growth, maxGrowth ) ->
            ( position, Triangle ( growth + dt |> min maxGrowth, maxGrowth ) )

        Player ->
            ( position, entity )


spawnSystem : World Entity LogicSystem RenderSystem -> World Entity LogicSystem RenderSystem
spawnSystem world =
    let
        resetSpawnTimer : Int -> Float -> Entity -> ( Float, Entity )
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
                Spawner ( cd, _ ) spawnEntity ->
                    if cd <= 0 && (entitiesInRange pos spawnEntity w |> Dict.size) < 10 then
                        World.addEntityRandomPos (Random.float (pos - 200) (pos + 200)) spawnEntity w

                    else
                        w

                _ ->
                    w
    in
    (World.getEntities world |> Dict.toList |> List.map Tuple.second)
        |> List.foldl applySpawner world
        |> World.mapEntities resetSpawnTimer


moveMinionTowardsNearest : (Entity -> Bool) -> Float -> Float -> Entity -> World Entity a c -> ( Float, Entity )
moveMinionTowardsNearest pred range position entity world =
    case entity of
        Minion velocity acceleration ->
            let
                trianglesInRange =
                    World.getEntitiesRange (World.getCameraPosition world) range world
                        |> Dict.filter (\_ ( _, data ) -> pred data)
                        |> Dict.toList
                        |> List.map Tuple.second
                        |> List.sortBy (\( p, _ ) -> World.relativeDistance p position (World.mapSize world) |> abs)
                        |> List.head
            in
            case trianglesInRange of
                Just ( pos, _ ) ->
                    ( position
                    , Minion velocity
                        (acceleration
                            + (World.relativeDistance position pos (World.mapSize world)
                                * 0.00001
                              )
                        )
                    )

                Nothing ->
                    ( position
                    , Minion velocity
                        (acceleration
                            + (World.relativeDistance position (World.getCameraPosition world) (World.mapSize world)
                                * 0.00001
                              )
                        )
                    )

        _ ->
            ( position, entity )


runLogicSystem : Float -> LogicSystem -> World Entity LogicSystem RenderSystem -> World Entity LogicSystem RenderSystem
runLogicSystem dt system world =
    case system of
        Spawn ->
            spawnSystem world

        Time ->
            World.mapEntities (timeSystem dt) world

        AI ->
            let
                aiSystem : Int -> Float -> Entity -> ( Float, Entity )
                aiSystem _ position entity =
                    moveMinionTowardsNearest isReadyTriangle 200 position entity world
            in
            World.mapEntities aiSystem world



-- RENDERSYSTEM


type RenderSystem
    = Position
    | Shape
    | Vector


runRenderSystem : Int -> Float -> Entity -> RenderSystem -> Svg Msg
runRenderSystem id position entity system =
    case system of
        Position ->
            Svg.g []
                [ Svg.text_
                    [ Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.fill "beige"
                    , Svg.Attributes.transform "translate(0, 50)"
                    ]
                    [ Svg.text ("pos: " ++ prettyFloat position) ]
                ]

        Vector ->
            case entity of
                Minion velocity _ ->
                    Svg.g []
                        [ Svg.line
                            [ Svg.Attributes.x1 (String.fromFloat 0)
                            , Svg.Attributes.y1 "0"
                            , Svg.Attributes.x2 (String.fromFloat (0 + (velocity * 100)))
                            , Svg.Attributes.y2 "0"
                            , Svg.Attributes.stroke "beige"
                            , Svg.Attributes.strokeWidth "3"
                            ]
                            []
                        ]

                _ ->
                    Svg.g [] []

        Shape ->
            viewEntity id entity



-- MODEL


type alias Model =
    World Entity LogicSystem RenderSystem


init : () -> ( Model, Cmd Msg )
init _ =
    ( World.empty
        -- |> World.addRenderSystem Position
        |> World.addRenderSystem Shape
        -- |> World.addRenderSystem Vector
        |> World.addLogicSystem Time
        |> World.addLogicSystem Spawn
        |> World.addLogicSystem AI
        |> World.addEntity 0 (Minion 0 0)
        |> World.addEntity 200 (Minion 0 0)
        |> World.addEntity 800 (Minion 0 0)
        |> World.addEntity 0 (Spawner ( 4000, 4000 ) (Triangle ( 0, 5000 )))
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
    main_ [ Html.Attributes.id "app" ]
        [ Html.div
            [ Html.Attributes.class "sidebar"
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
            , Svg.Attributes.style ("background-position: " ++ String.fromFloat -(World.getCameraPosition model) ++ "px 0")
            ]
            [ World.runRenderSystems 600 runRenderSystem model
            , viewEntity -1 Player
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 100 >> Tick)



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
