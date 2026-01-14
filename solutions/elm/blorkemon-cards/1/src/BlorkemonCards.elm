module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )


type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power


maxPower : Card -> Card -> Int
maxPower card1 card2 =
    [ card1, card2 ]
        |> List.map .power
        |> List.maximum
        |> Maybe.withDefault 0


sortByMonsterName : List Card -> List Card
sortByMonsterName cards =
    List.sortBy .monster cards


sortByCoolness : List Card -> List Card
sortByCoolness =
    let
        shiningToInt : Card -> Int
        shiningToInt card =
            if card.shiny then
                1

            else
                0

        coolness : Card -> ( Int, Int )
        coolness card =
            ( shiningToInt card, card.power )
    in
    List.reverse << List.sortBy coolness


compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    let
        powerComparison =
            compare card1.power card2.power
    in
    case powerComparison of
        EQ ->
            case ( card1.shiny, card2.shiny ) of
                ( True, False ) ->
                    GT

                ( False, True ) ->
                    LT

                _ ->
                    EQ

        _ ->
            powerComparison


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        LT ->
            card2.monster

        GT ->
            card1.monster

        EQ ->
            "too close to call"
