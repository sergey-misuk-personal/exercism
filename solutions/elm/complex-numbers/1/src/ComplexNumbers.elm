module ComplexNumbers exposing
    ( Complex
    , abs
    , add
    , conjugate
    , div
    , exp
    , fromPair
    , fromReal
    , imaginary
    , mul
    , real
    , sub
    )


type alias Complex =
    ( Float, Float )


fromPair : ( Float, Float ) -> Complex
fromPair pair =
    pair


fromReal : Float -> Complex
fromReal float =
    ( float, 0 )


real : Complex -> Float
real =
    Tuple.first


imaginary : Complex -> Float
imaginary =
    Tuple.second


conjugate : Complex -> Complex
conjugate ( a, b ) =
    ( a, -b )


abs : Complex -> Float
abs ( a, b ) =
    sqrt <| a ^ 2 + b ^ 2


add : Complex -> Complex -> Complex
add ( a, b ) ( c, d ) =
    ( a + c, b + d )


sub : Complex -> Complex -> Complex
sub ( a, b ) ( c, d ) =
    ( a - c, b - d )


mul : Complex -> Complex -> Complex
mul ( a, b ) ( c, d ) =
    ( a * c - b * d, b * c + a * d )


div : Complex -> Complex -> Complex
div ( a, b ) ( c, d ) =
    ( (a * c + b * d) / (c ^ 2 + d ^ 2), (b * c - a * d) / (c ^ 2 + d ^ 2) )


exp : Complex -> Complex
exp ( a, b ) =
    ( e ^ a * cos b, e ^ a * sin b )
