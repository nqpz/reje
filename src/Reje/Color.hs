module Reje.Color where

import Data.Ratio
import Data.Word
import Data.Bits
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import qualified Data.Colour.RGBSpace.HSV as HSV
import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Colour.Names as N


data Color = RGBA { r :: Double
                  , g :: Double
                  , b :: Double
                  , a :: Double
                  }
           deriving (Show, Read, Eq, Ord)


colorDoubleToWord :: (Bits w, Integral w) => Double -> w
colorDoubleToWord = round . (* 255.0)

colorWordToDouble :: (Bits w, Integral w) => w -> Double
colorWordToDouble w = fromIntegral w / 255.0

realFracToRealFrac :: (RealFrac a, RealFrac b) => a -> b
realFracToRealFrac = fromRational . toRational

fromRGBAw :: Word8 -> Word8 -> Word8 -> Word8 -> Color
fromRGBAw r g b a = RGBA (w r) (w g) (w b) (w a)
  where w = colorWordToDouble

fromColour :: RealFrac a => Colour a -> Color
fromColour colour = RGBA (c r) (c g) (c b) 1.0
  where RGB r g b = toRGB colour
        c = realFracToRealFrac

fromName :: String -> Maybe Color
fromName name = do
  colour <- N.readColourName name
  return $ fromColour colour

colorLight :: Color -> Double -> Color
colorLight (RGBA r g b a) light = RGBA r' g' b' a
  where rgb = RGB.RGB r g b
        valueNew = light * HSV.value rgb
        RGB.RGB r' g' b' = HSV.hsv (HSV.hue rgb) (HSV.saturation rgb) (valueNew)

aliceblue = fromColour N.aliceblue
antiquewhite = fromColour N.antiquewhite
aqua = fromColour N.aqua
aquamarine = fromColour N.aquamarine
azure = fromColour N.azure
beige = fromColour N.beige
bisque = fromColour N.bisque
black = fromColour N.black
blanchedalmond = fromColour N.blanchedalmond
blue = fromColour N.blue
blueviolet = fromColour N.blueviolet
brown = fromColour N.brown
burlywood = fromColour N.burlywood
cadetblue = fromColour N.cadetblue
chartreuse = fromColour N.chartreuse
chocolate = fromColour N.chocolate
coral = fromColour N.coral
cornflowerblue = fromColour N.cornflowerblue
cornsilk = fromColour N.cornsilk
crimson = fromColour N.crimson
cyan = fromColour N.cyan
darkblue = fromColour N.darkblue
darkcyan = fromColour N.darkcyan
darkgoldenrod = fromColour N.darkgoldenrod
darkgray = fromColour N.darkgray
darkgreen = fromColour N.darkgreen
darkgrey = fromColour N.darkgrey
darkkhaki = fromColour N.darkkhaki
darkmagenta = fromColour N.darkmagenta
darkolivegreen = fromColour N.darkolivegreen
darkorange = fromColour N.darkorange
darkorchid = fromColour N.darkorchid
darkred = fromColour N.darkred
darksalmon = fromColour N.darksalmon
darkseagreen = fromColour N.darkseagreen
darkslateblue = fromColour N.darkslateblue
darkslategray = fromColour N.darkslategray
darkslategrey = fromColour N.darkslategrey
darkturquoise = fromColour N.darkturquoise
darkviolet = fromColour N.darkviolet
deeppink = fromColour N.deeppink
deepskyblue = fromColour N.deepskyblue
dimgray = fromColour N.dimgray
dimgrey = fromColour N.dimgrey
dodgerblue = fromColour N.dodgerblue
firebrick = fromColour N.firebrick
floralwhite = fromColour N.floralwhite
forestgreen = fromColour N.forestgreen
fuchsia = fromColour N.fuchsia
gainsboro = fromColour N.gainsboro
ghostwhite = fromColour N.ghostwhite
gold = fromColour N.gold
goldenrod = fromColour N.goldenrod
gray = fromColour N.gray
grey = fromColour N.grey
green = fromColour N.green
greenyellow = fromColour N.greenyellow
honeydew = fromColour N.honeydew
hotpink = fromColour N.hotpink
indianred = fromColour N.indianred
indigo = fromColour N.indigo
ivory = fromColour N.ivory
khaki = fromColour N.khaki
lavender = fromColour N.lavender
lavenderblush = fromColour N.lavenderblush
lawngreen = fromColour N.lawngreen
lemonchiffon = fromColour N.lemonchiffon
lightblue = fromColour N.lightblue
lightcoral = fromColour N.lightcoral
lightcyan = fromColour N.lightcyan
lightgoldenrodyellow = fromColour N.lightgoldenrodyellow
lightgray = fromColour N.lightgray
lightgreen = fromColour N.lightgreen
lightgrey = fromColour N.lightgrey
lightpink = fromColour N.lightpink
lightsalmon = fromColour N.lightsalmon
lightseagreen = fromColour N.lightseagreen
lightskyblue = fromColour N.lightskyblue
lightslategray = fromColour N.lightslategray
lightslategrey = fromColour N.lightslategrey
lightsteelblue = fromColour N.lightsteelblue
lightyellow = fromColour N.lightyellow
lime = fromColour N.lime
limegreen = fromColour N.limegreen
linen = fromColour N.linen
magenta = fromColour N.magenta
maroon = fromColour N.maroon
mediumaquamarine = fromColour N.mediumaquamarine
mediumblue = fromColour N.mediumblue
mediumorchid = fromColour N.mediumorchid
mediumpurple = fromColour N.mediumpurple
mediumseagreen = fromColour N.mediumseagreen
mediumslateblue = fromColour N.mediumslateblue
mediumspringgreen = fromColour N.mediumspringgreen
mediumturquoise = fromColour N.mediumturquoise
mediumvioletred = fromColour N.mediumvioletred
midnightblue = fromColour N.midnightblue
mintcream = fromColour N.mintcream
mistyrose = fromColour N.mistyrose
moccasin = fromColour N.moccasin
navajowhite = fromColour N.navajowhite
navy = fromColour N.navy
oldlace = fromColour N.oldlace
olive = fromColour N.olive
olivedrab = fromColour N.olivedrab
orange = fromColour N.orange
orangered = fromColour N.orangered
orchid = fromColour N.orchid
palegoldenrod = fromColour N.palegoldenrod
palegreen = fromColour N.palegreen
paleturquoise = fromColour N.paleturquoise
palevioletred = fromColour N.palevioletred
papayawhip = fromColour N.papayawhip
peachpuff = fromColour N.peachpuff
peru = fromColour N.peru
pink = fromColour N.pink
plum = fromColour N.plum
powderblue = fromColour N.powderblue
purple = fromColour N.purple
red = fromColour N.red
rosybrown = fromColour N.rosybrown
royalblue = fromColour N.royalblue
saddlebrown = fromColour N.saddlebrown
salmon = fromColour N.salmon
sandybrown = fromColour N.sandybrown
seagreen = fromColour N.seagreen
seashell = fromColour N.seashell
sienna = fromColour N.sienna
silver = fromColour N.silver
skyblue = fromColour N.skyblue
slateblue = fromColour N.slateblue
slategray = fromColour N.slategray
slategrey = fromColour N.slategrey
snow = fromColour N.snow
springgreen = fromColour N.springgreen
steelblue = fromColour N.steelblue
tan = fromColour N.tan
teal = fromColour N.teal
thistle = fromColour N.thistle
tomato = fromColour N.tomato
turquoise = fromColour N.turquoise
violet = fromColour N.violet
wheat = fromColour N.wheat
white = fromColour N.white
whitesmoke = fromColour N.whitesmoke
yellow = fromColour N.yellow
yellowgreen = fromColour N.yellowgreen
