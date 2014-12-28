module Reje.Tracks where

import Reje.Track
import Reje.Random
import Reje.Color

import Data.Functor
import Control.Applicative
import Control.Monad


trackForw :: Track
trackForw = parseTrack $ cycle "FF"

trackHZig :: Track
trackHZig = track $ cycle (levo 20 ++ hovo 20 ++ rivo 20)

trackUp :: Track
trackUp = track $ cycle (houp 10 ++ hoow 30)

trackVZig :: Track
trackVZig = track $ cycle (hoow 20 ++ hovo 20 ++ houp 20)

trackZig :: Track
trackZig = track $ zip (cycle (ri 40 ++ le 40)) (cycle (up 30 ++ ow 30))

trackRand :: IO Track
trackRand = do
  seed <- evalRandIO (randomR minBound maxBound)
  let (dirs, colors) = flip evalRand (mkStdGen seed) $ do
        dirs_colors <- sequence $ repeat $ do
          dirs <- randomDirs
          color <- randomColor
          return (dirs, color)
        let dirs = concat $ fst $ unzip dirs_colors
            colors = snd $ unzip dirs_colors
        return (dirs, colors)
  return $ Track { dirs = dirs
                 , colors = colors
                 , useWalls = True
                 }

randomDirs :: RandomState [Dir]
randomDirs = do
  n <- randomR 5 50
  choice $ map ($ n) [ riup, riow, rivo, leup, leow, levo, houp, hoow, hovo ]

randomColor :: RandomState Color
randomColor = RGBA <$> d <*> d <*> d <*> pure 1
  where d = randomR 0.0 1.0
