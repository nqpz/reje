{-# LANGUAGE TupleSections #-}
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

trackBumps :: Track
trackBumps = track $ zip (cycle (ho 1)) (cycle (up 10 ++ ow 10 ++ vo 25))

trackRand :: IO Track
trackRand = evalRandIO $ do
  useWalls <- weightedChoice [(False, 10), (True, 1)] -- walls aren't that nice
  dirss_colors <- sequence $ repeat $ (,) <$> randomDirs <*> randomColor
  let dirs = concat $ fst $ unzip dirss_colors
      colors = snd $ unzip dirss_colors
  return $ Track { dirs = dirs
                 , colors = colors
                 , useWalls = useWalls
                 }

randomDirs :: RandomState [Dir]
randomDirs = do
  n <- randomR 5 50
  ($ n) <$> weightedChoice [ (riup, 2), (riow, 1), (rivo, 2)
                           , (leup, 2), (leow, 1), (levo, 2)
                           , (houp, 2), (hoow, 1), (hovo, 2)
                           ]

randomColor :: RandomState Color
randomColor = RGBA <$> d <*> d <*> d <*> pure 1
  where d = randomR 0.0 1.0
