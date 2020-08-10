module Main (main) where

import System.Environment (getArgs)
import System.Exit

import Reje.Game (play)
import Reje.Tracks


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
      "bumps" -> return trackBumps
      _ -> putStrLn "no such track" >> exitFailure
    _ -> trackRand
