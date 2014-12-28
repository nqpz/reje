module Reje.Game where

import Reje.Perspective
import Reje.Track
import Reje.Color
import Reje.Random

import Data.List
import Data.Ratio
import Data.Bits
import Data.Functor
import Control.Monad
import Control.Concurrent (threadDelay)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import Graphics.UI.SDL.Keysym
import Debug.Trace (trace)


type Polygon = [(Rational, Rational)]

data Object = Polygon Polygon Color
             deriving (Show, Eq)


debug :: Show a => String -> a -> a
debug s x = trace (s ++ ": " ++ show x) x

debug' :: Show b => (a -> b) -> String -> a -> a
debug' f s x = trace (s ++ ": " ++ show (f x)) x


w, h :: Integer
(w, h) = (1920, 1080)

trackObjects :: TrackAnnot -> [Object]
trackObjects (dirs, nPiece) =
  concat $ zipWith (\ps c -> map (\p -> Polygon p c) ps) polygonss colors
  where polygonss = map (uncurry makePoly)
                    $ map ((\(y, ((x00, x01, _, _), (x10, x11, _, _))) -> (y, ((x00, x01), (x10, x11)))))
                    $ filter ((\((_, _, (v0, _), _), (_, _, (v1, _), _)) -> v0 == Shown && v1 == Shown) . snd)
                    $ take 60 $ zip endYs' (zip xs (tail xs))
        colors = zipWith colorLight
                 (drop (fromIntegral (nPiece `mod` 2))
                  $ cycle [deepskyblue, ivory])
                 lightings
        lightings = map (fromRational . (1 -)) (0 : perspPoints 20)
        endYs' = zip (map (\(_, _, _, y) -> y) xs) (tail $ map (\(_, _, _, y) -> y) xs)
--        ys = map (\(y, e) -> (e * y)) $ zip (perspPointsUntil 20 (1 % h)) endYs'
        xs = debug' (take 10) "cur" $ scanl nextXs (0, fromIntegral w - 1, (Shown, fromIntegral w), 0) $ zip endXs endYs
        endXs = map stretchX $ map (endPoint 4) $ hturns $ fst $ unzip dirs
        endYs = map stretchY $ map (endPoint 4) $ vturns $ snd $ unzip dirs
        stretchY = (fromIntegral h *)
        stretchX = (fromIntegral w *)
        makePoly (y0, y1) ((x00, x01), (x10, x11)) =
--          if y0 > y1 then [] else
          [ [(x00, y0'), (x01, y0'), (x11, y1'), (x10, y1')]
          , [(x00, y0'), (x10, y1'), (x10, 0), (x00, 0)]
          , [(x01, y0'), (x11, y1'), (x11, 0), (x01, 0)]
          -- , [(x00, y0), (x01, y0), (x11, y1), (x10, y1)]
          -- , [(x00, y0'), (x10, y1'), (x10, y1), (x00, y0)]
          -- , [(x01, y0'), (x11, y1'), (x11, y1), (x01, y0)]
          ]
          where (y0', y1') = (fromIntegral h - y0, fromIntegral h - y1)

data Visibility = Shown
                | Hidden
                deriving (Show, Eq)

nextXs :: (Rational, Rational, (Visibility, Rational), Rational) -> (Rational, Rational)
          -> (Rational, Rational, (Visibility, Rational), Rational)
nextXs (x0, x1, (visib, odiff), yCur1) (xEnd, yEnd) = --if visib == Hidden then (x0, x1, visib) else
  (x0 + (xEnd - x0) * (yNext1 - yCur1) / (yEnd - yCur1),
   x1 + (xEnd - x1) * (yNext1 - yCur1) / (yEnd - yCur1),
   (Shown, odiff),--if yNext1 > yCur1 && x1 - x0 < odiff then (Shown, (x1 - x0)) else (Hidden, odiff),
   yNext1)
  where yNext1 = yCur1 + (yEnd - yCur1) / 30


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

nextTrackPiece :: [Dir] -> [Dir]
nextTrackPiece (_ : ps@(_ : _)) = ps
nextTrackPiece ps = ps


play :: Track -> IO ()
play track = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode (fromIntegral w) (fromIntegral h) 32 []
  SDL.setCaption "reje" "reje"
  screenSurf <- SDL.getVideoSurface
  play' screenSurf (track, 0)

play' :: SDL.Surface -> TrackAnnot -> IO ()
play' screenSurf track@(dirs, nPiece) = do
  renderTrackToScreen screenSurf track
  threadDelay 20000
  play' screenSurf $ (nextTrackPiece dirs, nPiece + 1)

renderTrackToScreen screenSurf track = do
  let format = SDL.surfaceGetPixelFormat screenSurf
  SDL.fillRect screenSurf Nothing =<< colorToPixel' format black
  mapM_ (drawObject screenSurf) $ reverse $ trackObjects track
  SDL.flip screenSurf

colorToPixel' :: SDL.PixelFormat -> Color -> IO SDL.Pixel
colorToPixel' format color = case color of
  RGBA r g b a -> SDL.mapRGBA format (w r) (w g) (w b) (w a)
  where w = colorDoubleToWord

colorToPixel :: Color -> SDL.Pixel
colorToPixel color = case color of
  RGBA r g b a -> SDL.Pixel (
    (colorDoubleToWord r) `shiftL` 24
    + (colorDoubleToWord g) `shiftL` 16
    + (colorDoubleToWord b) `shiftL` 8
    + (colorDoubleToWord a))

drawObject :: SDL.Surface -> Object -> IO ()
drawObject screenSurf (Polygon points color) = do
  putStrLn "(uu"
  putStrLn "(uu"
  putStrLn "(uu"
  void $ SDLp.filledPolygon screenSurf
    (map (\(a, b) -> (round a, round b)) points)
    $ colorToPixel color


trackForw :: Track
trackForw = cycle $ parseTrack "FF"

trackHZig :: Track
trackHZig = cycle (levo 20 ++ hovo 20 ++ rivo 20)

trackUp :: Track
trackUp = cycle (houp 10 ++ hoow 30)

trackVZig :: Track
trackVZig = cycle (hoow 20 ++ hovo 20 ++ houp 20)

trackZig :: Track
trackZig = zip (cycle (ri 40 ++ le 40)) (cycle (up 30 ++ ow 30))

trackRand :: IO Track
trackRand = do
  seed <- evalRandIO (randomR minBound maxBound)
  return $ evalRand (concat <$> (sequence $ repeat randomDirs)) $ mkStdGen seed

randomDirs :: RandomState [Dir]
randomDirs = do
  n <- randomR 5 50
  choice $ map ($ n) [ riup, riow, rivo, leup, leow, levo, houp, hoow, hovo ]
