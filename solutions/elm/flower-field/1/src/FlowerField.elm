module FlowerField exposing (annotate)

import Array exposing (Array)
import List
import Maybe exposing (Maybe)


type alias Garden =
    Array (Array Char)


type alias Index =
    ( Int, Int )


type alias Offset =
    Index


bomb : Char
bomb =
    '*'


offsets : List Offset
offsets =
    [ ( -1, -1 )
    , ( -1, 0 )
    , ( -1, 1 )
    , ( 0, -1 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    ]


countBombsAround : Garden -> Index -> Int
countBombsAround garden index =
    let
        ( row, column ) =
            index

        addOffset offset =
            let
                ( rowOffset, columnOffest ) =
                    offset
            in
            ( row + rowOffset, column + columnOffest )

        getCharAt : Index -> Maybe Char
        getCharAt i =
            let
                ( iR, iC ) =
                    i
            in
            Array.get iR garden
                |> Maybe.andThen (Array.get iC)
    in
    offsets
        |> List.filterMap (getCharAt << addOffset)
        |> List.filter ((==) bomb)
        |> List.length


countBombs : Garden -> Garden
countBombs garden =
    let
        digitToChar : Int -> Char
        digitToChar digit =
            Char.fromCode <| Char.toCode '0' + digit

        processItem : Int -> Int -> Char -> Char
        processItem row column char =
            if char == bomb then
                char

            else
                let
                    bombCount =
                        countBombsAround garden ( row, column )
                in
                if bombCount > 0 then
                    digitToChar bombCount

                else
                    char

        processRow : Int -> Array Char -> Array Char
        processRow row rowData =
            Array.indexedMap (processItem row) rowData
    in
    Array.indexedMap processRow garden


toArray : String -> Array (Array Char)
toArray =
    String.lines >> List.map String.toList >> List.map Array.fromList >> Array.fromList


fromArray =
    Array.toList >> List.map Array.toList >> List.map String.fromList >> String.join "\n"


annotate : String -> String
annotate garden =
    garden
        |> toArray
        |> countBombs
        |> fromArray
