module ImgLocation ( GpsCoord (..)
                   , ImgBounds (..)
                   , ImgCoord (..)
                   , locateIn) where

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
      n = toRadians (north img)
      s = toRadians (south img)
      w = toRadians (west img)
      e = toRadians (east img)
      yMin = latitudeToY s
      yMax = latitudeToY n
      xFactor = fromIntegral (width img) / (e - w)
      yFactor = fromIntegral (height img) / (yMax - yMin)
      x = (lon - w) * xFactor
      y = (yMax - latitudeToY lat) * yFactor in
      ImgCoord (round x) (round y)
