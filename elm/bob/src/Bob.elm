module Bob exposing (..)

{- Bob's replies:
   "" -> 'Fine. Be that way!'
   uppercase question -> 'Calm down, I know what I'm doing!'
   lowercase question -> 'Sure.'
   yell -> 'Whoa, chill out!'
   _ -> 'Whatever.'
-}


type S
    = Blank
    | Question
    | YelledQuestion
    | Yell
    | Other
    | Numbers


hey : String -> String
hey remark =
    renderS (List.foldl update Blank (String.toList remark))


renderS : S -> String
renderS s =
    case s of
        Blank ->
            "Fine. Be that way!"

        Question ->
            "Sure."

        YelledQuestion ->
            "Calm down, I know what I'm doing!"

        Yell ->
            "Whoa, chill out!"

        Other ->
            "Whatever."

        Numbers ->
            "Whatever."





update : Char -> S -> S
update c s =
    case s of
        Blank ->
            updateBlank c

        Yell ->
            updateYell c

        YelledQuestion ->
            updateYelledQuestion c

        Question ->
            updateQuestion c

        Other ->
            updateOther c

        Numbers ->
            updateNumbers c



updateBlank : Char -> S
updateBlank c =
    case isWhitespace c of
        True ->
            Blank

        False ->
            case Char.isUpper c || isPunctuation c of
                True ->
                    Yell

                False ->
                    case Char.isDigit c of
                        True ->
                            Numbers

                        False ->
                            Other


updateYell : Char -> S
updateYell c =
    case isWhitespace c || Char.isUpper c || Char.isDigit c || isPunctuation c of
        True ->
            Yell

        False ->
            case c == '?' of
                True ->
                    YelledQuestion

                False ->
                    Other


updateYelledQuestion : Char -> S
updateYelledQuestion c =
    case isWhitespace c of
        True ->
            YelledQuestion

        False ->
            case Char.isUpper c || isPunctuation c || Char.isDigit c of
                True ->
                    Yell

                False ->
                    Other


updateQuestion : Char -> S
updateQuestion c =
    case isWhitespace c of
        True ->
            Question

        False ->
            Other


updateOther : Char -> S
updateOther c =
    case isWhitespace c of
        True ->
            Other

        False ->
            case c == '?' of
                True ->
                    Question

                False ->
                    Other


updateNumbers : Char -> S
updateNumbers c =
    case isWhitespace c || Char.isDigit c || isPunctuation c of
        True ->
            Numbers

        False ->
            case c == '?' of
                True ->
                    Question

                False ->
                    case Char.isUpper c of
                        True ->
                            Yell

                        False ->
                            Other



isWhitespace : Char -> Bool
isWhitespace s =
    List.member s [ ' ', '\t', '\n', '\u{000D}' ]


isPunctuation : Char -> Bool
isPunctuation c =
    List.member c [ '.', ',', '!', '\'', '%', '^', '*', '@', '#', '$', '(', ')' ]



{- hey : String -> String
   hey remark =
       case allWhitespace remark of
           True ->
               "Fine. Be that way!"

           False ->
               case isQuestion remark of
                   True ->
                       case isYelling remark of
                           True ->
                               "Calm down, I know what I'm doing!"

                           False ->
                               "Sure."

                   False ->
                       case isYelling remark of
                           True ->
                               "Whoa, chill out!"

                           False ->
                               "Whatever."


   isQuestion : String -> Bool
   isQuestion q =
       questionHelper (String.toList q)


   questionHelper : List Char -> Bool
   questionHelper sentence =
       case sentence of
           [ a ] ->
               a == '?'

           a :: rest ->
               case allWhitespace (String.fromList rest) of
                   True ->
                       a == '?'

                   False ->
                       questionHelper rest

           _ ->
               False


   isYelling : String -> Bool
   isYelling s =
       List.foldr (&&)
           True
           (yellingHelper (String.toList s))


   yellingHelper : List Char -> List Bool
   yellingHelper list =
       case list of
           [] ->
               []

           _ ->
               case List.filter Char.isAlpha list of
                   [] ->
                       [ False ]

                   a :: rest ->
                       Char.isUpper a :: yellingHelper rest


   isWhitespace : Char -> Bool
   isWhitespace s =
       List.member s [' ', '\t', '\n', '\r']


   allWhitespace : String -> Bool
   allWhitespace s =
       List.foldr (&&) True (List.map isWhitespace (String.toList s))
-}
