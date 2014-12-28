module Reje.Game where

import Reje.Perspective
import Reje.Track
import Reje.Color
import Reje.Misc

import Data.List
import Data.Ratio
import Data.Bits
import Control.Monad
import Control.Concurrent (threadDelay)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import Graphics.UI.SDL.Keysym


type Polygon = [(Rational, Rational)]

data Object = Polygon Polygon Color
             deriving (Show, Eq)


w, h :: Integer
(w, h) = (1920, 1080)

w' = fromIntegral w
h' = fromIntegral h
factor :: Integer
factor = 30
factorTurn :: Integer
factorTurn = 10

trackLen = 90

trackObjects :: Track -> [Object]
trackObjects (Track dirs colors useWalls) =
  concat $ zipWith (\ps c -> map (\p -> Polygon p c) ps) polygonss lightedColors
  where polygonss = map (uncurry (makePoly useWalls))
                    $ take trackLen
                    $ zip (zip ys (tail ys)) (zip xs (tail xs))
        ys = map (\(_, _, y) -> y) raw
        xs = map (\(x0, x1, _) -> (x0, x1)) raw
        raw = scanl (nextXs factor) (0, w' - 1, 0) $ zip xEnds yEnds
        xEnds = map (w' *) $ map (endPoint factorTurn) $ hturns $ map hdir dirs
        yEnds = map (h' *) $ map (endPoint factorTurn) $ vturns $ map vdir dirs
        lightedColors = zipWith colorLight colors lightings
        lightings = map (fromRational . (1 -)) (0 : perspPoints factor)

nextXs :: Integer
          -> (Rational, Rational, Rational) -> (Rational, Rational)
          -> (Rational, Rational, Rational)
nextXs factor (x0, x1, yCur) (xEnd, yEnd) = (f x0, f x1, yNext)
  where yNext = yCur + (yEnd - yCur) / fromIntegral factor
        f x = x + (xEnd - x) * (yNext - yCur) / (yEnd - yCur)

hturns :: [HDir] -> [Integer]
hturns (GoRight : dirs) = 1 : map (+1) (hturns dirs)
hturns (GoLeft : dirs) = -1 : map (subtract 1) (hturns dirs)
hturns (HGoForward : dirs) = 0 : hturns dirs
hturns [] = []

vturns :: [VDir] -> [Integer]
vturns (GoUp : dirs) = 1 : map (+1) (vturns dirs)
vturns (GoDown : dirs) = -1 : map (subtract 1) (vturns dirs)
vturns (VGoForward : dirs) = 0 : vturns dirs
vturns [] = []

endPoint :: Integer -> Integer -> Rational
endPoint k offset
  | offset >= 0 = 1%2 + ((0 : perspPoints k) !! fromIntegral offset) / 2
  | otherwise = 1 - endPoint k (-offset)

makePoly :: Bool -> (Rational, Rational)
            -> ((Rational, Rational), (Rational, Rational))
            -> [Polygon]
makePoly useWalls (y0, y1) ((x00, x01), (x10, x11)) =
          [ [(x00, y0'), (x01, y0'), (x11, y1'), (x10, y1')] ]
          ++ if useWalls then
               [ [(x00, y0'), (x10, y1'), (x10, 0), (x00, 0)]
               , [(x01, y0'), (x11, y1'), (x11, 0), (x01, 0)]
               ]
             else []
          where (y0', y1') = (h' - y0, h' - y1)

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
  threadDelay 2000
  play' screenSurf $ nextTrackPiece track

nextTrackPiece :: Track -> Track
nextTrackPiece track = case (dirs track, colors track) of
  (_ : dirs'@(_ : _), _ : colors'@(_ : _)) -> track { dirs = dirs'
                                                    , colors = colors' }
  _ -> track

renderTrackToScreen screenSurf track = do
  let format = SDL.surfaceGetPixelFormat screenSurf
  SDL.fillRect screenSurf Nothing =<< colorToPixelRight format black
  mapM_ (drawObject screenSurf) $ reverse $ trackObjects track
  SDL.flip screenSurf

-- Only works in some cases.
colorToPixelRight :: SDL.PixelFormat -> Color -> IO SDL.Pixel
colorToPixelRight format color = case color of
  RGBA r g b a -> SDL.mapRGBA format (w r) (w g) (w b) (w a)
  where w = colorDoubleToWord

-- Works in some other cases.
colorToPixel :: Color -> SDL.Pixel
colorToPixel color = case color of
  RGBA r g b a -> SDL.Pixel (
    (colorDoubleToWord r) `shiftL` 24
    + (colorDoubleToWord g) `shiftL` 16
    + (colorDoubleToWord b) `shiftL` 8
    + (colorDoubleToWord a))

drawObject :: SDL.Surface -> Object -> IO ()
drawObject screenSurf (Polygon points color) = do
  void $ SDLp.filledPolygon screenSurf
    (map (\(a, b) -> (round a, round b)) points)
    $ colorToPixel color
