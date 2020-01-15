module PhoneNumber exposing (..)


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        pn =
            List.filter Char.isDigit (String.toList phoneNumber)
    in
    case pn of
        [] ->
            Nothing

        _ ->
            case validElevenDigitNumber pn of
                True ->
                    Just (String.fromList (List.drop 1 pn))

                False ->
                    case validTenDigitNumber pn of
                        True ->
                            Just (String.fromList pn)

                        False ->
                            Nothing


validTenDigitNumber : List Char -> Bool
validTenDigitNumber number =
    case number of
        [] ->
            False

        _ ->
            case List.length number == 10 of
                True ->
                    validN number

                _ ->
                    False


validElevenDigitNumber : List Char -> Bool
validElevenDigitNumber number =
    case number of
        [] ->
            False

        _ ->
            case List.length number == 11 of
                True ->
                    case number of
                        x :: rest ->
                            case x == '1' of
                                True ->
                                    validN rest

                                False ->
                                    False

                        _ ->
                            False

                False ->
                    False


validN : List Char -> Bool
validN number =
    case number of
        a :: b :: c :: d :: _ ->
            let
                a1 =
                    Char.toCode a

                d1 =
                    Char.toCode d
            in
            (a1 >= 50 && a1 <= 57) && (d1 >= 50 && d1 <= 57)

        _ ->
            False
