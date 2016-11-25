module ElTiempoEs (elTiempoEs) where

import Provider
import Codec.Picture
import Data.List as L
import Control.Arrow as A
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX

elTiempoEs :: Provider
elTiempoEs = Provider {
      imgBounds = radarBounds
    , pixelToprecip = pixelToMmh
    , imgUrls = radarImgUrls
  }

radarBounds :: ImgBounds
radarBounds = ImgBounds {
    north  = toRadians 44.165484
  , south  = toRadians 35.101074
  , west   = toRadians $ -9.981277
  , east   = toRadians 4.991540
  , width  = 680
  , height = 537
}

-- transforms a pixel to precipitaion in mm/h
pixelToMmh :: PixelRGB8 -> Precip
pixelToMmh p@(PixelRGB8 r g b) =
    if r == g && g == b
      then Rain 0.0
      else  let distances = A.first (distance p) <$> combinedLegend
                minDistance (d1, _) (d2, _) = compare d1 d2 in
                snd $ L.minimumBy minDistance distances

distance :: PixelRGB8 -> PixelRGB8 -> Float
distance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
    sqrt $ fromIntegral(r2 - r1)^2 + fromIntegral(g2 - g1)^2 + fromIntegral(b2 - b1)^2

combinedLegend :: [(PixelRGB8, Precip)]
combinedLegend = rainLegend ++ snowLegend ++ mixedLegend

rainLegend :: [(PixelRGB8, Precip)]
rainLegend = [
      (PixelRGB8 100 200   0, Rain   0.1)
    , (PixelRGB8  80 175   0, Rain   0.3)
    , (PixelRGB8  60 150   0, Rain   0.5)
    , (PixelRGB8  40 125   0, Rain   0.9)
    , (PixelRGB8  20 105   0, Rain   1.5)
    , (PixelRGB8 250 250   0, Rain   2.4)
    , (PixelRGB8 250 175   0, Rain   3.6)
    , (PixelRGB8 250 100   0, Rain  10.0)
    , (PixelRGB8 250   0   0, Rain  20.0)
    , (PixelRGB8 200   0  25, Rain  49.0)
    , (PixelRGB8 150   0  75, Rain 100.0)
    , (PixelRGB8 150   0 150, Rain 205.0)
    , (PixelRGB8 150  75 200, Rain 421.0)
    , (PixelRGB8 200 150 200, Rain 865.0)
  ]

snowLegend :: [(PixelRGB8, Precip)]
snowLegend = [
      (PixelRGB8 210 210 250, Snow 0.061)
    , (PixelRGB8 180 180 250, Snow 0.1  )
    , (PixelRGB8 150 150 250, Snow 0.15 )
    , (PixelRGB8 120 120 250, Snow 0.2  )
    , (PixelRGB8  90  90 250, Snow 0.3  )
    , (PixelRGB8  60  60 250, Snow 0.5  )
    , (PixelRGB8  30  30 210, Snow 0.7  )
    , (PixelRGB8   0   0 180, Snow 0.9  )
    , (PixelRGB8   0   0 150, Snow 2.0  )
    , (PixelRGB8   0   0 120, Snow 3.3  )
  ]

mixedLegend :: [(PixelRGB8, Precip)]
mixedLegend = [
      (PixelRGB8 250 225 225, Mixed   0.07)
    , (PixelRGB8 250 200 205, Mixed   0.1 )
    , (PixelRGB8 250 175 185, Mixed   0.3 )
    , (PixelRGB8 250 150 165, Mixed   0.5 )
    , (PixelRGB8 250 100 125, Mixed   1.5 )
    , (PixelRGB8 250  75 105, Mixed   2.3 )
    , (PixelRGB8 250  50  85, Mixed   3.6 )
    , (PixelRGB8 225  25  65, Mixed  10.0 )
    , (PixelRGB8 225  25  65, Mixed  10.0 )
    , (PixelRGB8 200   0  45, Mixed  20.0 )
    , (PixelRGB8 175   0  25, Mixed  49.0 )
    , (PixelRGB8 150   0   0, Mixed 100.0 )
  ]

-- Radar URL generation
lastImageTime :: POSIXTime -> POSIXTime
lastImageTime time =  let mins = toInteger $ floor $ time / 60.0 in
                          fromIntegral $ 60 * (mins - (mins `mod` 15))

radarImgTimes :: UTCTime -> [UTCTime]
radarImgTimes time = let t = utcTimeToPOSIXSeconds time
                         interval = 15 * 60 -- minutes
                         t0 = lastImageTime t - 13 * interval in
                         posixSecondsToUTCTime . (\i -> t0 + i * interval) <$> [1,2..25]

radarImgUrls :: UTCTime -> [(UTCTime, String)]
radarImgUrls time = let format = "http://data-4c21db65c81f6.s3.amazonaws.com/eltiempo/maps/\
                                  \%Y/%m/%d/weather/radar/spain/680x537/spain-weather-radar-%Y%m%d%H%M.jpg" in
                     (\t -> (t, formatTime defaultTimeLocale format t)) <$> radarImgTimes time
