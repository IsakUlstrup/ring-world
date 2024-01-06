module RingWorld exposing
    ( World
    , addEntity
    , addEntityRandomPos
    , addLogicSystem
    , addRenderSystem
    , camera
    , directionTo
    , empty
    , getCameraPosition
    , getEntities
    , getEntitiesRange
    , mapEntities
    , mapSize
    , moveCamera
    , relativeDistance
    , removeEntity
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


empty : World a b c
empty =
    World
        { entities = Dict.empty
        , logicSystems = Dict.empty
        , renderSystems = Dict.empty
        , idCounter = 0
        , seed = Random.initialSeed 42
        , mapSize = 2300
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


addEntityRandomPos : Generator Float -> a -> World a b c -> World a b c
addEntityRandomPos posGenerator data world =
    let
        ( generatedPos, newWorld ) =
            runGenerator posGenerator world
    in
    addEntity generatedPos data newWorld


mapEntities : (Int -> Float -> a -> ( Float, a )) -> World a b c -> World a b c
mapEntities f (World world) =
    World
        { world
            | entities =
                Dict.map
                    (\id ( pos, data ) ->
                        f id pos data
                            |> Tuple.mapFirst (clampPosition (World world))
                    )
                    world.entities
        }


getEntities : World a b c -> Dict Int ( Float, a )
getEntities (World world) =
    world.entities


getEntitiesRange : Float -> Float -> World a b c -> Dict Int ( Float, a )
getEntitiesRange position radius (World world) =
    let
        isInRange : Int -> ( Float, a ) -> Bool
        isInRange _ ( pos, _ ) =
            (relativeDistance position pos world.mapSize |> abs) <= radius
    in
    world.entities |> Dict.filter isInRange


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


directionTo : Float -> Float -> Float -> Float
directionTo startPosition endPosition circleSize =
    let
        relativeDist =
            if startPosition <= endPosition then
                endPosition - startPosition

            else
                circleSize - startPosition + endPosition
    in
    if relativeDist <= circleSize / 2.0 then
        1

    else
        -1


systemIsEnabled : ( Int, ( Bool, a ) ) -> Bool
systemIsEnabled ( _, ( enabled, _ ) ) =
    enabled


runRenderSystems : Float -> (Int -> Float -> a -> c -> Svg msg) -> World a b c -> Svg msg
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
                  , runSystem id pos entity system
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



-- applyCommand : Cmd a -> World a b c -> World a b c
-- applyCommand cmd world =
--     case cmd of
--         AddEntityRandomPos posGenerator data ->
--             let
--                 ( generatedPos, newWorld ) =
--                     runGenerator posGenerator world
--             in
--             addEntity generatedPos data newWorld
--         RemoveEntity id ->
--             removeEntity id world
-- applyCommands : List (Cmd a) -> World a b c -> World a b c
-- applyCommands cmds world =
--     List.foldl applyCommand world cmds
-- runLogicSystem : (Int -> Float -> a -> b -> ( ( Float, a ), List (Cmd a) )) -> b -> World a b c -> World a b c
-- runLogicSystem runSystem system (World world) =
--     let
--         ( entities, commands ) =
--             Dict.foldl addAge ( Dict.empty, [] ) world.entities
--         addAge : Int -> ( Float, a ) -> ( Dict Int ( Float, a ), List (Cmd a) ) -> ( Dict Int ( Float, a ), List (Cmd a) )
--         addAge id ( pos, data ) ( entityAcc, cmdAcc ) =
--             let
--                 ( e, cmd ) =
--                     runSystem id pos data system
--             in
--             ( Dict.insert id (e |> Tuple.mapFirst (clampPosition (World world))) entityAcc
--             , cmd ++ cmdAcc
--             )
--     in
--     -- World
--     --     { world
--     --         | entities =
--     --             Dict.map
--     --                 (\_ ( pos, data ) ->
--     --                     runSystem pos data system
--     --                         |> Tuple.mapFirst (clampPosition (World world))
--     --                 )
--     --                 world.entities
--     --     }
--     World { world | entities = entities } |> applyCommands commands


runLogicSystems : (b -> World a b c -> World a b c) -> World a b c -> World a b c
runLogicSystems runSystem (World world) =
    List.foldl runSystem
        (World world)
        (Dict.toList world.logicSystems
            |> List.filter systemIsEnabled
            |> List.map (Tuple.second >> Tuple.second)
        )
