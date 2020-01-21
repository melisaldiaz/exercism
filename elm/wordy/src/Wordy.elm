module Wordy exposing (answer)


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


answer : String -> Maybe Int
answer problem =
    case unsupported problem of
        a :: rest ->
            Nothing

        [] ->
            let
                opds : List Int
                opds =
                    getOperands problem

                opns : List Operation
                opns =
                    getOperations problem

                exp : ( List String, List String )
                exp =
                    verifiedExp problem []
            in
            case opns of
                [] ->
                    case opds of
                        [] ->
                            Nothing

                        [ a ] ->
                            Just a

                        _ ->
                            Nothing

                o :: os ->
                    case opds of
                        [] ->
                            Nothing

                        a :: rest ->
                            if String.isEmpty (String.concat (Tuple.second exp)) then
                                evaluate opds opns

                            else
                                Nothing



-- Given a string representing a math problem, it returns a list containing
-- the correctly paired operands and operations, and a list of leftovers.
--Example: "What is 5 plus 3?" -> (["5","plus","3"],[""])


verifiedExp : String -> List String -> ( List String, List String )
verifiedExp string acc =
    let
        s : List String
        s =
            String.words (unnecessary string)

        f : String -> String
        f x =
            case String.toInt x of
                Nothing ->
                    ""

                Just i ->
                    String.fromInt i
    in
    case ( s, acc ) of
        ( a :: b :: c :: rest, [] ) ->
            case f a == a of
                False ->
                    ( acc, s )

                True ->
                    case isOperation b of
                        False ->
                            ( acc, s )

                        True ->
                            case f c == c of
                                False ->
                                    ( acc, s )

                                True ->
                                    verifiedExp
                                        (String.concat (List.intersperse " " rest))
                                        [ a, b, c ]

        ( a :: b :: rest, _ :: _ ) ->
            case isOperation a of
                False ->
                    ( [], s )

                True ->
                    case f b == b of
                        False ->
                            ( [], s )

                        True ->
                            verifiedExp
                                (String.concat (List.intersperse " " rest))
                                (acc ++ [ a, b ])

        ( [ _ ], _ ) ->
            ( acc, s )

        _ ->
            ( [], s )



-- Given a list of operands and a list of operations, it
-- evaluates the full expression.


evaluate : List Int -> List Operation -> Maybe Int
evaluate ints ops =
    case ( ints, ops ) of
        ( [], [] ) ->
            Nothing

        ( _ :: _, _ :: _ ) ->
            case mulAndDiv ints ops of
                ( [ ans ], [] ) ->
                    Just ans

                ( x :: xs, y :: ys ) ->
                    addAndSub (x :: xs) (y :: ys)

                _ ->
                    Nothing

        ( [ a ], [] ) ->
            Just a

        _ ->
            Nothing



-- Given a list of operands and operations, it solves the
-- corresponding additions and subtractions, returning the
-- final answer, if possible.


addAndSub : List Int -> List Operation -> Maybe Int
addAndSub ints ops =
    case ops of
        o :: restOps ->
            case ints of
                f :: s :: restInts ->
                    evaluate
                        (getOperator o f s :: restInts)
                        restOps

                [ a ] ->
                    Just a

                [] ->
                    Nothing

        [] ->
            Nothing



-- Given a list of operands and operations, it solves the multiplications
-- and divisions, if any, and returns a tuple with the updated lists of
-- operands and operations, for further evaluation by other functions.
-- Example:  mulAndDiv [2,-4,6,7] [Addition,Multiplication,Subtraction]
--             -> ([2,-24,7],[Addition,Subtraction])


mulAndDiv : List Int -> List Operation -> ( List Int, List Operation )
mulAndDiv ints ops =
    case ( ints, ops ) of
        ( _ :: _, _ :: _ ) ->
            case find (\x -> x == Multiplication) ops of
                Just i ->
                    case ( get i ints, get (i + 1) ints ) of
                        ( Just a, Just b ) ->
                            mulAndDiv
                                (replace i (a * b) (dropAt (i + 1) ints))
                                (dropAt i ops)

                        _ ->
                            ( ints, ops )

                Nothing ->
                    case find (\x -> x == Division) ops of
                        Just i ->
                            case ( get i ints, get (i + 1) ints ) of
                                ( Just a, Just b ) ->
                                    mulAndDiv
                                        (replace i (a // b) (dropAt (i + 1) ints))
                                        (dropAt i ops)

                                _ ->
                                    ( ints, ops )

                        Nothing ->
                            ( ints, ops )

        ( [ ans ], [] ) ->
            ( [ ans ], [] )

        _ ->
            ( ints, ops )



-- Given a string representing a math problem, it returns
-- a list of all the operations mentioned, if any.
-- Example: "What is 3 plus 5?" -> [Addition]


getOperations : String -> List Operation
getOperations string =
    let
        s : List String
        s =
            String.words (unnecessary string)
    in
    Maybe.withDefault []
        (sequence (List.map toOperation (List.filter isOperation s)))



-- Determines whether a string contains an operation word.


isOperation : String -> Bool
isOperation s =
    case s of
        "plus" ->
            True

        "minus" ->
            True

        "multiplied" ->
            True

        "divided" ->
            True

        _ ->
            False



-- Given a string, it returns the corresponding operation, if possible.


toOperation : String -> Maybe Operation
toOperation s =
    case s of
        "plus" ->
            Just Addition

        "minus" ->
            Just Subtraction

        "multiplied" ->
            Just Multiplication

        "divided" ->
            Just Division

        _ ->
            Nothing



-- Given a string, it returns the corresponding operator, if possible.


getOperator : Operation -> (Int -> Int -> Int)
getOperator s =
    case s of
        Addition ->
            (+)

        Subtraction ->
            (-)

        Multiplication ->
            (*)

        Division ->
            (//)



-- Discards text that is unnecessary to process the math problem.


unnecessary : String -> String
unnecessary s =
    s
        |> String.replace "What is" ""
        |> String.replace "by" ""
        |> String.replace "?" ""



-- Given a string that contains a math problem, it returns a
-- list of terminology related to unsupported math operations,
-- if any.


unsupported : String -> List String
unsupported s =
    List.filter (\x -> String.contains x s) extra



-- Terminology for extra operations not supported by this module.


extra : List String
extra =
    [ "squared"
    , "cubed"
    , "power"
    , "raised"
    , "to the"
    , "root"
    ]



-- Given a string representing a math problem, it returns
-- a list of the operands mentioned, if any.
-- Example: "What is 3 multiplied by -4?"" -> [3,-4]


getOperands : String -> List Int
getOperands s =
    let
        op : List (Maybe Int)
        op =
            List.map String.toInt (String.words (unnecessary s))

        f : Maybe Int -> List Int -> List Int
        f m acc =
            case m of
                Nothing ->
                    acc

                Just x ->
                    acc ++ [ x ]
    in
    List.foldl f [] op


sequence : List (Maybe x) -> Maybe (List x)
sequence lm =
    case lm of
        [] ->
            Just []

        a :: rest ->
            Maybe.map2 (::) a (sequence rest)



-- Given a list and an element, returns the index of
-- the first matching element, if possible.


find : (a -> Bool) -> List a -> Maybe Int
find p l =
    case l of
        x :: xs ->
            case p x of
                True ->
                    Just 0

                False ->
                    Maybe.map ((+) 1) (find p xs)

        _ ->
            Nothing



-- Get the element at the position provided, if possible.


get : Int -> List a -> Maybe a
get pos list =
    case pos of
        0 ->
            case list of
                a :: rest ->
                    Just a

                _ ->
                    Nothing

        n ->
            case list of
                a :: rest ->
                    get (n - 1) rest

                _ ->
                    Nothing



-- Replace the element at the given position by the provided element.
-- If the position is out of range, it changes nothing.


replace : Int -> a -> List a -> List a
replace pos val la =
    case List.length la > pos of
        True ->
            List.take pos la ++ [ val ] ++ List.drop (pos + 1) la

        False ->
            la



-- Remove the nth element from a list.


dropAt : Int -> List a -> List a
dropAt n list =
    case list of
        [] ->
            []

        a :: rest ->
            List.take n list ++ List.drop (n + 1) list
