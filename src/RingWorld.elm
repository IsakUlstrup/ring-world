module RingWorld exposing
    ( Cmd
    , World
    , addEntity
    , addEntityRandomPosCmd
    , addLogicSystem
    , addRenderSystem
    , camera
    , empty
    , getCameraPosition
    , mapSize
    , moveCamera
    , removeEntityCmd
    , runLogicSystems
    , runRenderSystems
    , setCameraPos
    )

import Dict exposing (Dict)
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Keyed


type World a b c
    = World
        { entities : Dict Int ( Float, a )
        , logicSystems : Dict Int ( Bool, b )
        , renderSystems : Dict Int ( Bool, c )
        , idCounter : Int
        , seed : Seed
        , mapSize : Float
        , cameraPosition : Float
        }


type Cmd a
    = AddEntityRandomPos (Generator Float) a
    | RemoveEntity Int


addEntityRandomPosCmd : Generator Float -> a -> Cmd a
addEntityRandomPosCmd generator data =
    AddEntityRandomPos generator data


removeEntityCmd : Int -> Cmd a
removeEntityCmd id =
    RemoveEntity id


empty : World a b c
empty =
    World
        { entities = Dict.empty
        , logicSystems = Dict.empty
        , renderSystems = Dict.empty
        , idCounter = 0
        , seed = Random.initialSeed 42
        , mapSize = 1300
        , cameraPosition = 0
        }


mapSize : World a b c -> Float
mapSize (World world) =
    world.mapSize



-- ENTITY


addEntity : Float -> a -> World a b c -> World a b c
addEntity position entity (World world) =
    World
        { world
            | entities = Dict.insert world.idCounter ( clampPosition (World world) position, entity ) world.entities
            , idCounter = world.idCounter + 1
        }


removeEntity : Int -> World a b c -> World a b c
removeEntity id (World world) =
    World { world | entities = Dict.remove id world.entities }



-- RENDER


addRenderSystem : c -> World a b c -> World a b c
addRenderSystem system (World world) =
    World
        { world
            | renderSystems = Dict.insert world.idCounter ( True, system ) world.renderSystems
            , idCounter = world.idCounter + 1
        }


camera : World a b c -> List (Svg msg) -> Svg msg
camera (World world) children =
    Svg.g [ Svg.Attributes.transform ("translate(" ++ String.fromFloat -world.cameraPosition ++ ", 0)") ] children


getCameraPosition : World a b c -> Float
getCameraPosition (World world) =
    world.cameraPosition


clampPosition : World a b c -> Float -> Float
clampPosition (World world) position =
    if position > world.mapSize then
        position - world.mapSize

    else if position < 0 then
        world.mapSize + position

    else
        position


moveCamera : Float -> World a b c -> World a b c
moveCamera delta (World world) =
    let
        cameraPos =
            clampPosition (World world)
                (world.cameraPosition + delta)
    in
    World { world | cameraPosition = cameraPos }


setCameraPos : Float -> World a b c -> World a b c
setCameraPos pos (World world) =
    World { world | cameraPosition = pos }


relativeDistance : Float -> Float -> Float -> Float
relativeDistance startPosition endPosition circleSize =
    let
        relativeDist =
            if startPosition <= endPosition then
                endPosition - startPosition

            else
                circleSize - startPosition + endPosition
    in
    if relativeDist <= circleSize / 2.0 then
        relativeDist

    else
        -(circleSize - relativeDist)


systemIsEnabled : ( Int, ( Bool, a ) ) -> Bool
systemIsEnabled ( _, ( enabled, _ ) ) =
    enabled


runRenderSystems : Float -> (Float -> a -> c -> Svg msg) -> World a b c -> Svg msg
runRenderSystems renderRadius runSystem (World world) =
    let
        isInRange : Float -> ( Int, ( Float, a ) ) -> Bool
        isInRange radius ( _, ( pos, _ ) ) =
            (relativeDistance world.cameraPosition pos world.mapSize |> abs) <= radius

        renderEntity : c -> ( Int, ( Float, a ) ) -> Svg msg
        renderEntity system ( id, ( pos, entity ) ) =
            Svg.Keyed.node "g"
                [ Svg.Attributes.class "entity-transform"
                , Svg.Attributes.transform ("translate(" ++ String.fromFloat (relativeDistance world.cameraPosition pos world.mapSize) ++ ", 0)")
                ]
                [ ( String.fromInt id
                  , runSystem pos entity system
                  )
                ]

        runRenderSystem : ( Int, ( Bool, c ) ) -> ( String, Svg msg )
        runRenderSystem ( id, ( _, data ) ) =
            ( String.fromInt id
            , Svg.g [ Svg.Attributes.class ("render-system-" ++ String.fromInt id) ]
                (world.entities
                    |> Dict.toList
                    |> List.filter (isInRange renderRadius)
                    |> List.map (renderEntity data)
                )
            )
    in
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "render-systems" ]
        (Dict.toList world.renderSystems
            |> List.filter systemIsEnabled
            |> List.map runRenderSystem
        )



-- LOGIC


addLogicSystem : b -> World a b c -> World a b c
addLogicSystem system (World world) =
    World
        { world
            | logicSystems = Dict.insert world.idCounter ( True, system ) world.logicSystems
            , idCounter = world.idCounter + 1
        }


runGenerator : Generator x -> World a b c -> ( x, World a b c )
runGenerator generator (World world) =
    let
        ( value, newSeed ) =
            Random.step generator world.seed
    in
    ( value, World { world | seed = newSeed } )


applyCommand : Cmd a -> World a b c -> World a b c
applyCommand cmd world =
    case cmd of
        AddEntityRandomPos posGenerator data ->
            let
                ( generatedPos, newWorld ) =
                    runGenerator posGenerator world
            in
            addEntity generatedPos data newWorld

        RemoveEntity id ->
            removeEntity id world


applyCommands : List (Cmd a) -> World a b c -> World a b c
applyCommands cmds world =
    List.foldl applyCommand world cmds


runLogicSystem : (Int -> Float -> a -> b -> ( ( Float, a ), List (Cmd a) )) -> b -> World a b c -> World a b c
runLogicSystem runSystem system (World world) =
    let
        ( entities, commands ) =
            Dict.foldl addAge ( Dict.empty, [] ) world.entities

        addAge : Int -> ( Float, a ) -> ( Dict Int ( Float, a ), List (Cmd a) ) -> ( Dict Int ( Float, a ), List (Cmd a) )
        addAge id ( pos, data ) ( entityAcc, cmdAcc ) =
            let
                ( e, cmd ) =
                    runSystem id pos data system
            in
            ( Dict.insert id (e |> Tuple.mapFirst (clampPosition (World world))) entityAcc
            , cmd ++ cmdAcc
            )
    in
    -- World
    --     { world
    --         | entities =
    --             Dict.map
    --                 (\_ ( pos, data ) ->
    --                     runSystem pos data system
    --                         |> Tuple.mapFirst (clampPosition (World world))
    --                 )
    --                 world.entities
    --     }
    World { world | entities = entities } |> applyCommands commands


runLogicSystems : (Int -> Float -> a -> b -> ( ( Float, a ), List (Cmd a) )) -> World a b c -> World a b c
runLogicSystems runSystem (World world) =
    List.foldl (runLogicSystem runSystem)
        (World world)
        (Dict.toList world.logicSystems
            |> List.filter systemIsEnabled
            |> List.map (Tuple.second >> Tuple.second)
        )
