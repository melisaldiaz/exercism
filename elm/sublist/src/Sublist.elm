module Sublist exposing (ListComparison(..), sublist)


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist l1 l2 =
    if l1 == l2 then
        Equal

    else if isSublist l1 l2 then
        Sublist

    else if isSublist l2 l1 then
        Superlist

    else
        Unequal


isSublist : List a -> List a -> Bool
isSublist la lb =
    let
        f : List a -> List a -> Bool
        f l1 l2 =
            case ( l1, l2 ) of
                ( [], [] ) ->
                    True

                ( [], _ :: _ ) ->
                    True

                ( a :: rest1, b :: rest2 ) ->
                    case a == b of
                        True ->
                            f rest1 rest2

                        False ->
                            isSublist la (List.drop 1 lb)

                ( a :: rest1, [] ) ->
                    False
    in
    if List.length la <= List.length lb then
        f la lb

    else
        False
