module Reje.Track where

data Dir = GoForward
         | TurnLeft
         | TurnRight
         deriving (Eq, Show)

data Track = Track [Dir] Integer
           deriving (Eq, Show)

track :: [Dir] -> Track
track dirs = Track dirs 0

parseTrack :: String -> Track
parseTrack = track . concatMap conv
  where conv 'F' = [GoForward]
        conv 'L' = [TurnLeft]
        conv 'R' = [TurnRight]
        conv _ = []
