module Gps where

type Latitude = Double
type Longitude = Double

data GpsPoint = GpsPoint
        { latitude      :: Latitude
        , longitude     :: Longitude
        }
        deriving (Eq, Ord, Show, Read)

zeroPoint = GpsPoint 0 0
