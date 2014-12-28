module Reje.Perspective where

import Data.List
import Data.Ratio


recurseBuild :: (a -> a) -> a -> [a]
recurseBuild step prev = prev : recurseBuild' prev
  where recurseBuild' prev = next : recurseBuild' next
          where next = step prev

perspStart :: Integer -> Rational
perspStart k = 1 % k

perspStep :: Integer -> Rational -> Rational
perspStep k prev = prev - prev / fromIntegral k

perspPoints :: Integer -> [Rational]
perspPoints k = scanl1 (+) $ recurseBuild (perspStep k) (perspStart k)

perspPairs :: Integer -> [(Rational, Rational)]
perspPairs k = zip (0 : points) points
  where points = perspPoints k

perspPairsUntil :: Integer -> Rational -> [(Rational, Rational)]
perspPairsUntil k threshold =
  takeWhile (\(a, b) -> b - a > threshold) $ perspPairs k

perspPointsUntil :: Integer -> Rational -> [Rational]
perspPointsUntil k threshold = map snd $ perspPairsUntil k threshold
