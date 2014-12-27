module Reje.Game where

import Reje.Perspective
import Reje.Track
import Reje.Color

import Data.List
import Data.Ratio
import Data.Bits
import Control.Concurrent (threadDelay)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import Graphics.UI.SDL.Keysym

type Polygon = [(Rational, Rational)]

data Object = Polygon Polygon Color
             deriving (Show, Eq)

w, h :: Integer
(w, h) = (1920, 1080)

yEnd = h % 2

trackToPoly :: Track -> [Object]
trackToPoly (Track dirs nPiece) =
  concat $ zipWith (\ps c -> map (\p -> Polygon p c) ps) polygonss colors
  where polygonss = zipWith makePoly ys (zip xs (tail xs))
        colors = zipWith colorLight
                 (drop (fromIntegral (nPiece `mod` 2))
                  $ cycle [deepskyblue, ivory])
                 lightings
        lightings = map (fromRational . (1 -)) (0 : perspPoints 20)
        ys = map (\(a, b) -> (stretchY a, stretchY b)) $ perspPairsUntil 20 (1 % h)
        xs = scanl nextXs (0, fromIntegral w - 1) $ zip endXs ys
        endXs = map stretchX $ map (endPoint 4) $ turns dirs
        stretchY = (yEnd *)
        stretchX = (fromIntegral w *)
        makePoly (y0, y1) ((x00, x01), (x10, x11)) =
          [ [(x00, y0'), (x01, y0'), (x11, y1'), (x10, y1')]
          , [(x00, y0), (x01, y0), (x11, y1), (x10, y1)]
          , [(x00, y0'), (x10, y1'), (x10, y1), (x00, y0)]
          , [(x01, y0'), (x11, y1'), (x11, y1), (x01, y0)]
          ]
          where (y0', y1') = (fromIntegral h - y0, fromIntegral h - y1)

nextXs :: (Rational, Rational) -> (Rational, (Rational, Rational))
          -> (Rational, Rational)
nextXs (x0, x1) (xEnd, (yCur, yNext)) =
  (x0 + (xEnd - x0) * (yNext - yCur) / (yEnd - yCur),
   x1 + (xEnd - x1) * (yNext - yCur) / (yEnd - yCur))

turns :: [Dir] -> [Integer]
turns (TurnRight : dirs) = 1 : map (+1) (turns dirs)
turns (TurnLeft : dirs) = -1 : map (subtract 1) (turns dirs)
turns (GoForward : dirs) = 0 : turns dirs
turns [] = []

endPoint :: Integer -> Integer -> Rational
endPoint k offset
  | offset >= 0 = 1%2 + ((0 : perspPoints k) !! fromIntegral offset) / 2
  | otherwise = 1 - endPoint k (-offset)

nextTrackPiece :: [Dir] -> [Dir]
nextTrackPiece (p : ps@(_ : _)) = ps
nextTrackPiece ps = ps


play :: Track -> IO ()
play track = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode (fromIntegral w) (fromIntegral h) 32 []
  SDL.setCaption "reje" "reje"
  screenSurf <- SDL.getVideoSurface
  play' screenSurf track

play' :: SDL.Surface -> Track -> IO ()
play' screenSurf track = do
  renderTrackToScreen screenSurf track
  readEvent screenSurf track

renderTrackToScreen screenSurf track = do
  SDL.fillRect screenSurf Nothing (SDL.Pixel 0x00000000)
  mapM_ (drawObject screenSurf) (reverse $ trackToPoly track)
  SDL.flip screenSurf

-- Doesn't work for some reason.
-- colorToPixel :: SDL.PixelFormat -> Color -> IO SDL.Pixel
-- colorToPixel format color = case color of
--   RGBA r g b a -> SDL.mapRGBA format (w r) (w g) (w b) (w a)
--   where w = colorDoubleToWord8

colorToPixel :: Color -> SDL.Pixel
colorToPixel color = case color of
  RGBA r g b a -> SDL.Pixel (
    (w r) `shiftL` 24
    + (w g) `shiftL` 16
    + (w b) `shiftL` 8
    + (w a))
  where w = colorDoubleToWord

drawObject :: SDL.Surface -> Object -> IO Bool
drawObject screenSurf (Polygon points color) = do
  let pixel = colorToPixel color
  SDLp.filledPolygon screenSurf
    (map (\(a, b) -> (int16 a, int16 b)) points) pixel
  where int16 = round

readEvent screenSurf track@(Track dirs nPiece) = do
  threadDelay 20000
  play' screenSurf $ Track (nextTrackPiece dirs) (nPiece + 1)
--   event <- SDL.pollEvent
--   case eventAction event of
--     Nothing -> SDL.quit
--     Just Nothing -> play' screenSurf track
--     Just (Just action) -> play' screenSurf $ Track (action dirs) (nPiece + 1)

-- eventAction :: SDL.Event -> Maybe (Maybe ([Dir] -> [Dir]))
-- eventAction (SDL.KeyDown (Keysym k mods _))
--   | k == SDLK_UP = Just $ Just nextTrackPiece
-- eventAction SDL.Quit = Nothing
-- eventAction _ = Just Nothing

-- hasCtrl mods = any (`elem` mods)
--                [KeyModCtrl, KeyModLeftCtrl, KeyModRightCtrl]



track0 :: Track
track0 = parseTrack "FFFRRFFFFFFLFLF"

track1 :: Track
track1 = track $ cycle (ri 20 ++ fo 20 ++ le 20)

track2 :: Track
track2 = track $ cycle (ri 60 ++ fo 40)

track3 :: Track
track3 = track $ cycle (ri 100 ++ le 100)

track4 :: Track
track4 = track $ cycle (fo 2)

ri :: Int -> [Dir]
ri n = take n $ repeat TurnRight

le :: Int -> [Dir]
le n = take n $ repeat TurnLeft

fo :: Int -> [Dir]
fo n = take n $ repeat GoForward
