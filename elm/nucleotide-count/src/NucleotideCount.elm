module NucleotideCount exposing (..)


type Nucleotide
    = A
    | C
    | G
    | T


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


initialNuc : NucleotideCounts
initialNuc =
    { a = 0, t = 0, c = 0, g = 0 }


nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    case String.uncons sequence of
        Nothing ->
            initialNuc

        Just ( a, rest ) ->
            case toNucleotide a of
                Nothing ->
                    nucleotideCounts rest

                Just x -> updateCount (nucleotideCounts rest) x
                    

updateCount : NucleotideCounts -> Nucleotide -> NucleotideCounts
updateCount nc n =
    case n of
        A ->
            { nc | a = nc.a + 1 }

        C ->
            { nc | c = nc.c + 1 }

        G ->
            { nc | g = nc.g + 1 }

        T ->
            { nc | t = nc.t + 1 }


toNucleotide : Char -> Maybe Nucleotide
toNucleotide c =
    case c of
        'A' ->
            Just A

        'C' ->
            Just C

        'G' ->
            Just G

        'T' ->
            Just T

        _ ->
            Nothing


