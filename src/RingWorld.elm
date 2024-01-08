module RingWorld exposing
    ( World
    , addEntity
    , addEntityRandomPos
    , addLogicSystem
    , addRenderSystem
    , camera
    , directionTo
    , empty
    , getEntities
    , getEntitiesRange
    , getMapSize
    , getPlayerPosition
    , mapEntities
    , movePlayer
    , relativeDistance
    , removeEntity
    , runLogicSystems
    , runRenderSystems
    , setPlayerPos
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
        , player : ( Float, a )
        }


empty : ( Float, a ) -> World a b c
empty ( playerPos, playerData ) =
    World
        { entities = Dict.empty
        , logicSystems = Dict.empty
        , renderSystems = Dict.empty
        , idCounter = 1
        , seed = Random.initialSeed 42
        , mapSize = 2300
        , player = ( clampPosition 2300 playerPos, playerData )
        }


getMapSize : World a b c -> Float
getMapSize (World world) =
    world.mapSize



-- ENTITY


addEntity : Float -> a -> World a b c -> World a b c
addEntity position entity (World world) =
    World
        { world
            | entities = Dict.insert world.idCounter ( clampPosition world.mapSize position, entity ) world.entities
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
                            |> Tuple.mapFirst (clampPosition world.mapSize)
                    )
                    world.entities
        }


getEntities : World a b c -> Dict Int ( Float, a )
getEntities (World world) =
    world.entities |> Dict.insert 0 world.player


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
camera world children =
    Svg.g [ Svg.Attributes.transform ("translate(" ++ String.fromFloat -(getPlayerPosition world) ++ ", 0)") ] children


getPlayerPosition : World a b c -> Float
getPlayerPosition (World world) =
    world.player |> Tuple.first


clampPosition : Float -> Float -> Float
clampPosition mapSize position =
    if position > mapSize then
        position - mapSize

    else if position < 0 then
        mapSize + position

    else
        position


movePlayer : Float -> World a b c -> World a b c
movePlayer delta (World world) =
    World { world | player = Tuple.mapFirst (\p -> clampPosition world.mapSize (p + delta)) world.player }


setPlayerPos : Float -> World a b c -> World a b c
setPlayerPos pos (World world) =
    World { world | player = Tuple.mapFirst (always (clampPosition world.mapSize pos)) world.player }


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
            (relativeDistance (world.player |> Tuple.first) pos world.mapSize |> abs) <= radius

        renderEntity : c -> ( Int, ( Float, a ) ) -> Svg msg
        renderEntity system ( id, ( pos, entity ) ) =
            Svg.Keyed.node "g"
                [ Svg.Attributes.class "entity-transform"
                , Svg.Attributes.transform ("translate(" ++ String.fromFloat (relativeDistance (world.player |> Tuple.first) pos world.mapSize) ++ ", 0)")
                ]
                [ ( String.fromInt id
                  , runSystem id pos entity system
                  )
                ]

        runRenderSystem : ( Int, ( Bool, c ) ) -> ( String, Svg msg )
        runRenderSystem ( id, ( _, data ) ) =
            ( String.fromInt id
            , Svg.g [ Svg.Attributes.class ("render-system-" ++ String.fromInt id) ]
                (World world
                    |> getEntities
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
