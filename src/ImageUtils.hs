module ImageUtils ( GpsCoord (..)
                   , ImgBounds (..)
                   , ImgCoord (..)
                   , locateIn
                   , toRadians
                   , readPixelFrom) where

import Codec.Picture
import Data.Map as M

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

readPixelFrom :: FilePath -> ImgCoord -> IO (Either String PixelRGB8)
readPixelFrom path (ImgCoord x y) = ((\ i -> pixelRGB8At i x y) <$>) <$> readImage path

pixelRGB8At :: DynamicImage -> Int -> Int -> PixelRGB8
pixelRGB8At img = pixelAt $ convertRGB8 img
