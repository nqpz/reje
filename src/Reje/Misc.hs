module Reje.Misc where

import Debug.Trace (trace)


debug :: Show a => String -> a -> a
debug s x = trace (s ++ ": " ++ show x) x

debug' :: Show b => (a -> b) -> String -> a -> a
debug' f s x = trace (s ++ ": " ++ show (f x)) x
