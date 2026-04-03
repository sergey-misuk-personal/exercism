module ProteinTranslation exposing (Error(..), proteins)

import Dict


type Error
    = InvalidCodon


methionine =
    "Methionine"


phenylalanine =
    "Phenylalanine"


leucine =
    "Leucine"


serine =
    "Serine"


tyrosine =
    "Tyrosine"


cysteine =
    "Cysteine"


tryptophan =
    "Tryptophan"


stop =
    "STOP"


codonToAminoAcid =
    Dict.fromList
        [ ( "AUG", methionine )
        , ( "UUU", phenylalanine )
        , ( "UUC", phenylalanine )
        , ( "UUA", leucine )
        , ( "UUG", leucine )
        , ( "UCU", serine )
        , ( "UCC", serine )
        , ( "UCA", serine )
        , ( "UCG", serine )
        , ( "UAU", tyrosine )
        , ( "UAC", tyrosine )
        , ( "UGU", cysteine )
        , ( "UGC", cysteine )
        , ( "UGG", tryptophan )
        , ( "UAA", stop )
        , ( "UAG", stop )
        , ( "UGA", stop )
        ]


proteins : String -> Result Error (List String)
proteins strand =
    if String.isEmpty strand then
        Ok []

    else
        let
            slice : String
            slice =
                String.slice 0 3 strand

            maybeAcid : Maybe String
            maybeAcid =
                Dict.get slice codonToAminoAcid
        in
        case maybeAcid of
            Nothing ->
                Err InvalidCodon

            Just acid ->
                if acid == stop then
                    Ok []

                else
                    let
                        strandTail =
                            String.dropLeft 3 strand

                        otherAcids =
                            proteins strandTail
                    in
                    case otherAcids of
                        Err _ ->
                            otherAcids

                        Ok acids ->
                            Ok <| acid :: acids
