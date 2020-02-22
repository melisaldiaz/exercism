{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Clock
  ( addDelta
  , fromHourMin
  , toString
  ) where

data Clock =
  Clock Int
        Int
  deriving (Eq, Show)

-- Converts two given numbers into a Clock,
-- with the hours and minutes properly wrapped.
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min' = uncurry Clock (wrapHM hour min')

-- Shows a Clock as a String with the format "hh:mm".
toString :: Clock -> String
toString (Clock h m) =
  case (showH h, showM m) of
    (Just a, Just b) -> a ++ ":" ++ b
    _                -> ""

-- Adds a duration, expressed in hours and minutes, to a
-- given time, represented by an instance of Clock.
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min' (Clock h m) = uncurry Clock (wrapHM (hour + h) (min' + m))

-- Show hours as a String.
showH :: Int -> Maybe String
showH 24 = Just "00"
showH n
  | n < 10 && n >= 0 = Just ('0' : show n)
  | n >= 10 && n < 24 = Just (show n)
  | otherwise = Nothing

-- Show minutes as a String.
showM :: Int -> Maybe String
showM 60 = Just "00"
showM n
  | n < 10 && n >= 0 = Just ('0' : show n)
  | n >= 10 && n < 60 = Just (show n)
  | otherwise = Nothing

-- Wraps hours.
wrapH :: Int -> Int
wrapH h = mod h 24

-- Wraps minutes.
wrapM :: Int -> Int
wrapM m = mod m 60

-- Wraps hours and minutes.
wrapHM :: Int -> Int -> (Int, Int)
wrapHM h m
  | m < 0 = (wrapH ((wrapH h) - wh), wrapM m)
  | otherwise = (wrapH (h + (div m 60)), wrapM m)
  where
    wh :: Int
    wh = wrapH (abs (floor (fromIntegral m / 60 :: Double)))
