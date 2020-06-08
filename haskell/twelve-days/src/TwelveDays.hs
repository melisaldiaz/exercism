{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TwelveDays
  ( recite,
  )
where

import Data.Text (Text)

recite :: Int -> Int -> [String]
recite start stop =
  case traverse recite' [start .. stop] of
    Nothing -> []
    Just song -> song

recite' :: Int -> Maybe String
recite' n
  | n > 0 && n < 13 =
    Just $
      beginning
        ++ (days !! (n - 1))
        ++ continuation
        ++ concat (reverse $ take n gifts)
  | otherwise = Nothing

beginning :: String
beginning = "On the "

continuation :: String
continuation = " day of Christmas my true love gave to me: "

days :: [String]
days =
  [ "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
    "eleventh",
    "twelfth"
  ]

gifts :: [String]
gifts =
  [ "a Partridge in a Pear Tree.",
    "two Turtle Doves, and ",
    "three French Hens, ",
    "four Calling Birds, ",
    "five Gold Rings, ",
    "six Geese-a-Laying, ",
    "seven Swans-a-Swimming, ",
    "eight Maids-a-Milking, ",
    "nine Ladies Dancing, ",
    "ten Lords-a-Leaping, ",
    "eleven Pipers Piping, ",
    "twelve Drummers Drumming, "
  ]
