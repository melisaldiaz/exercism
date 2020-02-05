module Robot
  ( Bearing(East, North, South, West)
  , bearing
  , coordinates
  , mkRobot
  , move
  ) where

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

data Robot = Robot
  { dir   :: Bearing
  , coord :: (Integer, Integer)
  }

bearing :: Robot -> Bearing
bearing robot = dir robot

coordinates :: Robot -> (Integer, Integer)
coordinates robot = coord robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction coordinates

move :: Robot -> String -> Robot
move robot instructions =
  case instructions of
    "" -> robot
    a:rest ->
      case a of
        'R' -> move (turnRight robot) rest
        'L' -> move (turnLeft robot) rest
        'A' -> move (advance robot) rest
        _   -> move robot rest

turnRight :: Robot -> Robot
turnRight robot =
  case dir robot of
    North -> robot {dir = East}
    East  -> robot {dir = South}
    South -> robot {dir = West}
    West  -> robot {dir = North}

turnLeft :: Robot -> Robot
turnLeft robot =
  case dir robot of
    North -> robot {dir = West}
    West  -> robot {dir = South}
    South -> robot {dir = East}
    East  -> robot {dir = North}

advance :: Robot -> Robot
advance robot =
  robot
    { coord =
        let (x, y) = coord robot
         in case dir robot of
              North -> (x, y + 1)
              South -> (x, y - 1)
              East  -> (x + 1, y)
              West  -> (x - 1, y)
    }
