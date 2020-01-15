module Allergies exposing (..)

import Bitwise


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


allergyMask : Allergy -> Int
allergyMask a =
    case a of
        Eggs ->
            1

        Peanuts ->
            2

        Shellfish ->
            4

        Strawberries ->
            8

        Tomatoes ->
            16

        Chocolate ->
            32

        Pollen ->
            64

        Cats ->
            128


allergies : List Allergy
allergies =
    [ Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats ]


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    Bitwise.and (allergyMask allergy) score /= 0 


toList : Int -> List Allergy
toList score =
    List.filter (\a -> isAllergicTo a score) allergies
