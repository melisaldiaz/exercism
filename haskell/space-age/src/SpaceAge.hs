{-# LANGUAGE LambdaCase #-}

module SpaceAge
  ( Planet(..)
  , ageOn
  ) where

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune


-- | Given an Earth age in seconds, it calculates 
-- how old someone would be on another planet.    
ageOn 
  :: Planet 
  -> Rational  -- ^ Earth age in seconds.
  -> Rational
ageOn planet seconds = 
  (seconds / orbitalPeriod planet) / toRational earthYear


-- | One Earth year in seconds.
earthYear :: Integer
earthYear = 31557600


-- | Orbital period in a given planet in 
-- terms of one Earth year.
orbitalPeriod :: Planet -> Rational
orbitalPeriod =
  \case
    Mercury -> 0.2408467
    Venus -> 0.61519726
    Earth -> 1.0
    Mars -> 1.8808158
    Jupiter -> 11.862615
    Saturn -> 29.447498
    Uranus -> 84.016846
    Neptune -> 164.79132
