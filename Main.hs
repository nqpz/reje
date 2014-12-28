module Main (main) where

import System.Environment (getArgs)
import System.Exit

import Reje.Game


main :: IO ()
main = do
  args <- getArgs
  play =<< case args of
    [trackId] -> case trackId of
      "forw" -> return trackForw
      "hzig" -> return trackHZig
      "up" -> return trackUp
      "vzig" -> return trackVZig
      "zig" -> return trackZig
      _ -> putStrLn "no such track" >> exitFailure
    _ -> trackRand
