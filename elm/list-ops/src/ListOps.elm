module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )


length : List a -> Int
length list =
    let
        f : List a -> Int -> Int
        f l n =
            case l of
                [] ->
                    n

                a :: rest ->
                    f rest (n + 1)
    in
    f list 0


reverse : List a -> List a
reverse list =
    let
        f : List a -> List a -> List a
        f l1 l2 =
            case l1 of
                [] ->
                    l2

                a :: rest ->
                    f rest (a :: l2)
    in
    f list []


foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    case list of
        [] ->
            acc

        a :: rest ->
            foldl f (f a acc) rest


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    case list of
        [] ->
            acc

        a :: rest ->
            f a (foldr f acc rest)


map : (a -> b) -> List a -> List b
map f list =
    case list of
        [] ->
            []

        a :: rest ->
            f a :: map f rest


filter : (a -> Bool) -> List a -> List a
filter f list =
    case list of
        [] ->
            []

        a :: rest ->
            case f a of
                True ->
                    a :: filter f rest

                False ->
                    filter f rest


append : List a -> List a -> List a
append xs ys =
    xs ++ ys


concat : List (List a) -> List a
concat list =
    case list of
        [] ->
            []

        a :: rest ->
            a ++ concat rest
