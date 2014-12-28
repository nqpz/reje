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
