module RomanNumerals exposing (..)


toRoman : Int -> String
toRoman n0 =
    let
        f : ( Int, String ) -> ( String, Int ) -> ( String, Int )
        f x acc =
            case ( x, acc ) of
                ( ( divisor, roman ), ( output, n1 ) ) ->
                    let
                        quot =
                            n1 // divisor

                        rem =
                            remainderBy divisor n1
                    in
                    ( output ++ String.repeat quot roman, rem )
    in
    Tuple.first (List.foldl f ( "", n0 ) numerals)


numerals : List ( Int, String )
numerals =
    [ ( 1000, "M" )
    , ( 900, "CM" )
    , ( 500, "D" )
    , ( 400, "CD" )
    , ( 100, "C" )
    , ( 90, "XC" )
    , ( 50, "L" )
    , ( 40, "XL" )
    , ( 10, "X" )
    , ( 9, "IX" )
    , ( 5, "V" )
    , ( 4, "IV" )
    , ( 1, "I" )
    ]
