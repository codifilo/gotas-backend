module Location where

data Coordinates = Coordinates {
    latitude  :: Double
  , longitude :: Double
} deriving (Eq, Ord, Show, Read)

data MapImage = MapImage {
    north  :: Double
  , south  :: Double
  , west   :: Double
  , east   :: Double
  , width  :: Int
  , height :: Int
} deriving (Eq, Ord, Show, Read)

data Pixel = Pixel {
    x :: Int
  , y :: Int
} deriving (Eq, Ord, Show, Read)

toRadians :: Double -> Double
toRadians d = d * (pi / 180)

toDegrees :: Double -> Double
toDegrees r = r * (180 / pi)

latitudeToY :: Double -> Double
latitudeToY lat = log $ tan (lat / 2 + pi / 4)

mapImage :: MapImage
mapImage = MapImage {
    north  = toRadians 44.165484
  , south  = toRadians 35.101074
  , west   = toRadians (-9.981277)
  , east   = toRadians 4.991540
  , width  = 680
  , height = 537
}

locateInMap :: MapImage -> Coordinates -> Pixel
locateInMap img coord =
  let lat = toRadians (latitude coord)
      lon = toRadians (longitude coord)
      yMin = latitudeToY (south img)
      yMax = latitudeToY (north img)
      xFactor = fromIntegral (width img) / (east img - west img)
      yFactor = fromIntegral (height img) / (yMax - yMin)
      x = (lon - west img) * xFactor
      y = (yMax - latitudeToY lat) * yFactor in
      Pixel (round x) (round y)
