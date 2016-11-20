module Provider (GpsCoord (..)
                 , ImgBounds (..)
                 , Provider (..)
                 , toRadians
                 , amountAt) where

import Codec.Picture
import Data.Map as M
import Data.Time.Clock.POSIX

data GpsCoord = GpsCoord {
    latitude  :: Double
  , longitude :: Double
} deriving (Eq, Ord, Show, Read)

data ImgBounds = ImgBounds {
    north  :: Double
  , south  :: Double
  , west   :: Double
  , east   :: Double
  , width  :: Int
  , height :: Int
} deriving (Eq, Ord, Show, Read)

data ImgCoord = ImgCoord {
    x :: Int
  , y :: Int
} deriving (Eq, Ord, Show, Read)

data Provider = Provider {
    imgBounds     :: ImgBounds
  , pixelToAmount :: PixelRGB8 -> Float
  , imgUrls       :: POSIXTime -> [String]
}

toRadians :: Double -> Double
toRadians d = d * (pi / 180)

latitudeToY :: Double -> Double
latitudeToY lat = log $ tan (lat / 2 + pi / 4)

locateIn :: ImgBounds -> GpsCoord -> ImgCoord
locateIn img coord =
  let lat = toRadians (latitude coord)
      lon = toRadians (longitude coord)
      yMin = latitudeToY $ south img
      yMax = latitudeToY $ north img
      xFactor = fromIntegral (width img) / (east img - west img)
      yFactor = fromIntegral (height img) / (yMax - yMin)
      x = (lon - west img) * xFactor
      y = (yMax - latitudeToY lat) * yFactor in
      ImgCoord (round x) (round y)

amountAt :: Provider -> DynamicImage -> GpsCoord -> Float
amountAt prov img gpsCoord =
  let (ImgCoord x y) = locateIn (imgBounds prov) gpsCoord
      pixel = pixelAt (convertRGB8 img) x y in
      pixelToAmount prov pixel
