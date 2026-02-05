module Meetup exposing (Month(..), Week(..), Weekday(..), meetup)

import Time exposing (Posix, utc)


type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


type Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


type Week
    = First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth


millisInDay : Int
millisInDay =
    24 * 60 * 60 * 1000


addDay : Time.Posix -> Time.Posix
addDay =
    Time.posixToMillis >> (+) millisInDay >> Time.millisToPosix


toYear =
    Time.toYear utc


toMonth : Time.Posix -> Month
toMonth posix =
    case Time.toMonth utc posix of
        Time.Jan ->
            January

        Time.Feb ->
            February

        Time.Mar ->
            March

        Time.Apr ->
            April

        Time.May ->
            May

        Time.Jun ->
            June

        Time.Jul ->
            July

        Time.Aug ->
            August

        Time.Sep ->
            September

        Time.Oct ->
            October

        Time.Nov ->
            November

        Time.Dec ->
            December


toMonthNumber : Month -> Int
toMonthNumber month =
    case month of
        January ->
            1

        February ->
            2

        March ->
            3

        April ->
            4

        May ->
            5

        June ->
            6

        July ->
            7

        August ->
            8

        September ->
            9

        October ->
            10

        November ->
            11

        December ->
            12


toDay =
    Time.toDay utc


toWeekday : Posix -> Weekday
toWeekday posix =
    case Time.toWeekday utc posix of
        Time.Mon ->
            Monday

        Time.Tue ->
            Tuesday

        Time.Wed ->
            Wednesday

        Time.Thu ->
            Thursday

        Time.Fri ->
            Friday

        Time.Sat ->
            Saturday

        Time.Sun ->
            Sunday


monthDates : Int -> Month -> List Posix
monthDates year month =
    let
        approximateDate =
            Time.millisToPosix <| ((year - 1970) * 365 + (toMonthNumber month - 1) * 29) * millisInDay

        adjustDate date =
            if toYear date == year && toMonth date == month && toDay date == 1 then
                date

            else
                adjustDate <| addDay date

        collectMonthDates : Posix -> List Posix
        collectMonthDates date =
            if toMonth date == month then
                date :: (collectMonthDates <| addDay date)

            else
                []
    in
    approximateDate
        |> adjustDate
        |> collectMonthDates


monthWeekdays : Int -> Month -> Weekday -> List Time.Posix
monthWeekdays year month weekday =
    let
        dates =
            monthDates year month

        correctWeekday =
            toWeekday >> (==) weekday
    in
    List.filter correctWeekday dates


select : List Posix -> Week -> Maybe Posix
select days week =
    let
        get : Int -> List a -> Maybe a
        get index list =
            if index == 0 then
                List.head list

            else
                Maybe.andThen (get <| index - 1) <| List.tail list

        teenth date =
            let
                day =
                    toDay date
            in
            day >= 13 && day <= 19
    in
    case week of
        First ->
            get 0 days

        Second ->
            get 1 days

        Third ->
            get 2 days

        Fourth ->
            get 3 days

        Last ->
            get 0 <| List.reverse days

        Teenth ->
            get 0 <| List.filter teenth days


meetup : Int -> Month -> Week -> Weekday -> String
meetup year month week weekday =
    let
        weekdays =
            monthWeekdays year month weekday

        date =
            case select weekdays week of
                Just d ->
                    d

                Nothing ->
                    Time.millisToPosix 0
    in
    [ String.fromInt <| toYear date
    , String.pad 2 '0' <| String.fromInt <| toMonthNumber (toMonth date)
    , String.pad 2 '0' <| String.fromInt <| toDay date
    ]
        |> List.intersperse "-"
        |> String.concat
