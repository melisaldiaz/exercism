module Tests exposing (tests)

import Expect
import Luhn exposing (valid)
import Test exposing (Test, describe, skip, test)


tests : Test
tests =
    describe "Luhn"
        [ test "single digit strings can not be valid" <|
            \_ ->
                Expect.equal False (valid "1")
        , 
            test "a single zero is invalid" <|
                \_ ->
                    Expect.equal False (valid "0")
        , 
            test "a simple valid SIN that remains valid if reversed" <|
                \_ ->
                    Expect.equal True (valid "059")
        , 
            test "a simple valid SIN that becomes invalid if reversed" <|
                \_ ->
                    Expect.equal True (valid "59")
        , 
            test "a valid Canadian SIN" <|
                \_ ->
                    Expect.equal True (valid "055 444 285")
        , 
            test "invalid Canadian SIN" <|
                \_ ->
                    Expect.equal False (valid "055 444 286")
        , 
            test "invalid credit card" <|
                \_ ->
                    Expect.equal False (valid "8273 1232 7352 0569")
        , 
            test "valid strings with a non-digit included become invalid" <|
                \_ ->
                    Expect.equal False (valid "055a 444 285")
        , 
            test "valid strings with punctuation included become invalid" <|
                \_ ->
                    Expect.equal False (valid "055-444-285")
        , 
            test "valid strings with symbols included become invalid" <|
                \_ ->
                    Expect.equal False (valid "055£ 444$ 285")
        , 
            test "single zero with space is invalid" <|
                \_ ->
                    Expect.equal False (valid " 0")
        , 
            test "more than a single zero is valid" <|
                \_ ->
                    Expect.equal True (valid "0000 0")
        , 
            test "input digit 9 is correctly converted to output digit 9" <|
                \_ ->
                    Expect.equal True (valid "091")
        ]
