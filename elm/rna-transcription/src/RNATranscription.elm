module RNATranscription exposing (..)


type Nucleotide
    = Adenine
    | Cytosine
    | Guanine
    | Thymine
    | Uracil


toRNA : String -> Result Char String
toRNA dna =
    case sequenceR (List.map toNucleotideR (String.toList dna)) of
        Err c ->
            Err c

        Ok ns ->
            Ok (String.fromList (List.map (complement >> fromNucleotide) ns))


fromNucleotide : Nucleotide -> Char
fromNucleotide n =
    case n of
        Adenine ->
            'A'

        Cytosine ->
            'C'

        Guanine ->
            'G'

        Thymine ->
            'T'

        Uracil ->
            'U'


toNucleotide : Char -> Maybe Nucleotide
toNucleotide c =
    case c of
        'A' ->
            Just Adenine

        'C' ->
            Just Cytosine

        'G' ->
            Just Guanine

        'T' ->
            Just Thymine

        'U' ->
            Just Uracil

        _ ->
            Nothing


toNucleotideR : Char -> Result Char Nucleotide
toNucleotideR c =
    Result.fromMaybe c (toNucleotide c) 
    


complement : Nucleotide -> Nucleotide
complement n =
    case n of
        Guanine ->
            Cytosine

        Cytosine ->
            Guanine

        Thymine ->
            Adenine

        Adenine ->
            Uracil

        Uracil ->
            Adenine


sequenceR : List (Result x y) -> Result x (List y)
sequenceR lr =
    case lr of
        [] ->
            Ok []

        a :: rest ->
            Result.map2 (::) a (sequenceR rest)
