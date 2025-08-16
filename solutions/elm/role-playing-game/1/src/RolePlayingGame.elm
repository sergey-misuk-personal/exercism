module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    case name of
        Nothing -> "Mighty Magician"
        Just n -> n


revive : Player -> Maybe Player
revive player =
    let
        mana =
            if player.level >= 10 then
                Just 100
            else
                Nothing
    in
    if player.health == 0 then
        Just {player | health = 100, mana = mana}
    else
        Nothing


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Just m ->
            if manaCost <= m then
                ({player | mana = Just (m - manaCost)}, 2 * manaCost)
            else
                (player, 0)
        Nothing ->
            if player.health >= manaCost then
                ({player | health = player.health - manaCost}, 0)
            else
                ({player | health = 0}, 0)
