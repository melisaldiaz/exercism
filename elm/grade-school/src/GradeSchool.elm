module GradeSchool exposing (..)

import Dict exposing (Dict)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Grade (List Student)


empty : School
empty =
    Dict.empty



addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    let
        f : Maybe (List Student) -> Maybe (List Student)
        f x =
            case x of
                Nothing ->
                    Just [ student ]

                Just v ->
                    Just (student :: v)
    in
    Dict.update grade f school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    case Dict.get grade school of
        Nothing ->
            []

        Just v ->
            List.sort v


allStudents : School -> List ( Grade, List Student )
allStudents school =
    let
        f : ( Grade, List Student ) -> ( Grade, List Student )
        f =
            \x -> ( Tuple.first x, List.sort (Tuple.second x) )
    in
    List.map f (Dict.toList school)
