module Strain exposing (discard, keep)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    case list of
        x::xs -> if predicate x then
                     x::keep predicate xs
                 else
                     keep predicate xs
        [] -> []


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    let
        invertedPredicate x =
            if predicate x then
                False
            else
                True
    in
    keep invertedPredicate list
