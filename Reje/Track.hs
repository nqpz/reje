module Reje.Track where

import Reje.Color

import Data.Maybe


data HDir = HGoForward
          | GoLeft
          | GoRight
          deriving (Eq, Show)

data VDir = VGoForward
          | GoDown
          | GoUp
          deriving (Eq, Show)

type Dir = (HDir, VDir)

hdir :: Dir -> HDir
hdir = fst

vdir :: Dir -> VDir
vdir = snd

type TrackBase = [Dir]

data Track = Track { dirs :: TrackBase
                   , colors :: [Color]
                   , useWalls :: Bool
                   }
           deriving (Eq, Show)


track :: TrackBase -> Track
track dirs = Track dirs (cycle [dimgray, lavender]) True

parseTrack :: String -> Track
parseTrack = track . parse' hconv vconv
  where hconv 'F' = Just HGoForward
        hconv 'L' = Just GoLeft
        hconv 'R' = Just GoRight
        hconv _ = Nothing

        vconv 'F' = Just VGoForward
        vconv 'D' = Just GoDown
        vconv 'U' = Just GoUp
        vconv _ = Nothing

        parse' :: (Char -> Maybe HDir)
                  -> (Char -> Maybe VDir)
                  -> String -> TrackBase
        parse' hf vf s = case splitAt 2 s of
          ([h, v], s') -> (++ parse' hf vf s') $ fromMaybe [] $ do
            hd <- hf h
            vd <- vf v
            return [(hd, vd)]
          _ -> []


ri :: Int -> [HDir]
ri n = take n $ repeat GoRight

le :: Int -> [HDir]
le n = take n $ repeat GoLeft

ho :: Int -> [HDir]
ho n = take n $ repeat HGoForward

up :: Int -> [VDir]
up n = take n $ repeat GoUp

ow :: Int -> [VDir]
ow n = take n $ repeat GoDown

vo :: Int -> [VDir]
vo n = take n $ repeat VGoForward

riup :: Int -> TrackBase
riup n = zip (ri n) (up n)

riow :: Int -> TrackBase
riow n = zip (ri n) (ow n)

rivo :: Int -> TrackBase
rivo n = zip (ri n) (vo n)

leup :: Int -> TrackBase
leup n = zip (le n) (up n)

leow :: Int -> TrackBase
leow n = zip (le n) (ow n)

levo :: Int -> TrackBase
levo n = zip (le n) (vo n)

houp :: Int -> TrackBase
houp n = zip (ho n) (up n)

hoow :: Int -> TrackBase
hoow n = zip (ho n) (ow n)

hovo :: Int -> TrackBase
hovo n = zip (ho n) (vo n)
