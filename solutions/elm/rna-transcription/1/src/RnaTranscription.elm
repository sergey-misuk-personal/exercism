module RnaTranscription exposing (toRNA)

import Dict exposing (Dict)


dnaToRna : Dict Char Char
dnaToRna =
    Dict.fromList
        [ ( 'G', 'C' )
        , ( 'C', 'G' )
        , ( 'T', 'A' )
        , ( 'A', 'U' )
        ]


convertOrPropagateError : Char -> Result String (List Char) -> Result String (List Char)
convertOrPropagateError char result =
    case ( result, Dict.get char dnaToRna ) of
        ( Ok o, Just c ) ->
            Ok (c :: o)

        ( Err _, _ ) ->
            result

        ( _, Nothing ) ->
            Err ""


toRNA : String -> Result String String
toRNA dna =
    dna
        |> String.toList
        |> List.foldr convertOrPropagateError (Ok [])
        |> Result.map String.fromList
