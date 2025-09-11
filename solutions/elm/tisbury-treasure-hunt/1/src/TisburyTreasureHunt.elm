module TisburyTreasureHunt exposing (..)

-- Consider defining a type alias for TreasureLocation,
-- Treasure, PlaceLocation and Place,
-- and using them in the function type annotations


type alias PlaceLocation =
    ( Char, Int )


type alias Place =
    ( String, PlaceLocation )


type alias TreasureLocation =
    ( Int, Char )


type alias Treasure =
    ( String, TreasureLocation )


placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation placeLocation =
    let
        ( c, i ) =
            placeLocation
    in
    ( i, c )


treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
    let
        ( c0, i0 ) =
            placeLocation

        ( i1, c1 ) =
            treasureLocation
    in
    c0 == c1 && i0 == i1


countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures place treasures =
    let
        ( _, placeLocation ) =
            place

        countTreasure ( _, treasureLocation ) =
            if treasureLocationMatchesPlaceLocation placeLocation treasureLocation then
                1

            else
                0
    in
    List.sum <| List.map countTreasure treasures


specialCaseSwapPossible : Treasure -> Place -> Treasure -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case ( foundTreasure, place ) of
        ( "Brass Spyglass", "Abandoned Lighthouse" ) ->
            True

        ( "Amethyst Octopus", "Stormy Breakwater" ) ->
            List.member desiredTreasure [ "Crystal Crab", "Glass Starfish" ]

        ( "Vintage Pirate Hat", "Harbor Managers Office" ) ->
            List.member desiredTreasure [ "Model Ship in Large Bottle", "Antique Glass Fishnet Float" ]

        _ ->
            False
