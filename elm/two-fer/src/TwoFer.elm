module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
    let
        s =
            case name of
                Nothing ->
                    "you"

                Just "" ->
                    "you"

                Just x ->
                    x
    in
    render s


render : String -> String
render x =
    "One for " ++ x ++ ", one for me."
