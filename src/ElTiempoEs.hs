module ElTiempoEs (elTiempoEs) where

import Provider
import Codec.Picture
import Codec.Picture.Types
import Data.List as L
import Control.Arrow as A
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Control.Concurrent
import Cache

elTiempoEs :: Provider
elTiempoEs = Provider {
      imgBounds = radarBounds
    , pixelToprecip = pixelToMmh
    , imgUrls = radarImgUrls
    , cacheInvalidationLoop = invalidateCacheAfterNextLoop
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
pixelToMmh rgb =
  let hsl@(HSL h s l) = toHSL rgb in
    if s < 10.0
      then None
      else  let distances = A.first (hueDistance hsl) <$> combinedLegend
                minDistance (d1, _) (d2, _) = compare d1 d2 in
                snd $ L.minimumBy minDistance distances

hueDistance :: HSL -> HSL -> Double
hueDistance (HSL h1 _ _) (HSL h2 _ _) = abs $ h1 - h2

data HSL = HSL {
      hue        :: Double
    , saturation :: Double
    , lightness  :: Double
} deriving (Eq, Ord, Show, Read)

toHSL :: PixelRGB8 -> HSL
toHSL (PixelRGB8 r g b) =
  let r' = fromIntegral r / 255.0
      g' = fromIntegral g / 255.0
      b' = fromIntegral b / 255.0
      max' = max r' $ max g' b'
      min' = min r' $ min g' b'
      l = (max' + min') / 2.0
      d = max' - min'
      s | max' == min' = 0.0
        | l > 0.5 = d / (2 - max' - min')
        | otherwise = d / (max' + min')
      h | max' == min' = 0.0
        | max' == r' = (g' - b') / d + (if g < b then 6 else 0)
        | max' == g' = (b' - r') / d + 2
        | max' == b' = (r' - g') / d + 4
        | otherwise = 0.0 in
      HSL (h*100+0.5) (s*100+0.5) (l*100+0.5)

combinedLegend :: [(HSL, Precip)]
combinedLegend = rainLegend ++ snowLegend ++ mixedLegend

rainLegend :: [(HSL, Precip)]
rainLegend = [
      (toHSL $ PixelRGB8 100 200   0, Rain   0.1)
    , (toHSL $ PixelRGB8  80 175   0, Rain   0.3)
    , (toHSL $ PixelRGB8  60 150   0, Rain   0.5)
    , (toHSL $ PixelRGB8  40 125   0, Rain   0.9)
    , (toHSL $ PixelRGB8  20 105   0, Rain   1.5)
    , (toHSL $ PixelRGB8 250 250   0, Rain   2.4)
    , (toHSL $ PixelRGB8 250 175   0, Rain   3.6)
    , (toHSL $ PixelRGB8 250 100   0, Rain  10.0)
    , (toHSL $ PixelRGB8 250   0   0, Rain  20.0)
    , (toHSL $ PixelRGB8 200   0  25, Rain  49.0)
    , (toHSL $ PixelRGB8 150   0  75, Rain 100.0)
    , (toHSL $ PixelRGB8 150   0 150, Rain 205.0)
    , (toHSL $ PixelRGB8 150  75 200, Rain 421.0)
    , (toHSL $ PixelRGB8 200 150 200, Rain 865.0)
  ]

snowLegend :: [(HSL, Precip)]
snowLegend = [
      (toHSL $ PixelRGB8 210 210 250, Snow 0.061)
    , (toHSL $ PixelRGB8 180 180 250, Snow 0.1  )
    , (toHSL $ PixelRGB8 150 150 250, Snow 0.15 )
    , (toHSL $ PixelRGB8 120 120 250, Snow 0.2  )
    , (toHSL $ PixelRGB8  90  90 250, Snow 0.3  )
    , (toHSL $ PixelRGB8  60  60 250, Snow 0.5  )
    , (toHSL $ PixelRGB8  30  30 210, Snow 0.7  )
    , (toHSL $ PixelRGB8   0   0 180, Snow 0.9  )
    , (toHSL $ PixelRGB8   0   0 150, Snow 2.0  )
    , (toHSL $ PixelRGB8   0   0 120, Snow 3.3  )
  ]

mixedLegend :: [(HSL, Precip)]
mixedLegend = [
      (toHSL $ PixelRGB8 250 225 225, Mixed   0.07)
    , (toHSL $ PixelRGB8 250 200 205, Mixed   0.1 )
    , (toHSL $ PixelRGB8 250 175 185, Mixed   0.3 )
    , (toHSL $ PixelRGB8 250 150 165, Mixed   0.5 )
    , (toHSL $ PixelRGB8 250 100 125, Mixed   1.5 )
    , (toHSL $ PixelRGB8 250  75 105, Mixed   2.3 )
    , (toHSL $ PixelRGB8 250  50  85, Mixed   3.6 )
    , (toHSL $ PixelRGB8 225  25  65, Mixed  10.0 )
    , (toHSL $ PixelRGB8 225  25  65, Mixed  10.0 )
    , (toHSL $ PixelRGB8 200   0  45, Mixed  20.0 )
    , (toHSL $ PixelRGB8 175   0  25, Mixed  49.0 )
    , (toHSL $ PixelRGB8 150   0   0, Mixed 100.0 )
  ]

-- Radar URL generation
intervalMins = 15
intervalSecs = intervalMins * secsPerMin
secsPerMin = 60
pastImgsCount = 11
futureImgsCount = 12

lastImagePOSIXTime :: POSIXTime -> POSIXTime
lastImagePOSIXTime time =  let mins = toInteger $ floor $ time / fromInteger secsPerMin in
                          fromIntegral $ secsPerMin * (mins - (mins `mod` intervalMins))

nextImagePOSIXTime :: POSIXTime -> POSIXTime
nextImagePOSIXTime time =  let mins = toInteger $ floor $ time / fromInteger secsPerMin in
                          fromIntegral $ secsPerMin * (mins + (intervalMins - (mins `mod` intervalMins)))

lastImageUTCTime :: UTCTime -> UTCTime
lastImageUTCTime = posixSecondsToUTCTime . lastImagePOSIXTime . utcTimeToPOSIXSeconds

nextImageUTCTime :: UTCTime -> UTCTime
nextImageUTCTime = posixSecondsToUTCTime . nextImagePOSIXTime . utcTimeToPOSIXSeconds

radarImgTimes :: UTCTime -> [UTCTime]
radarImgTimes time = pastImgsTimes time ++ futureImgsTimes time

pastImgsTimes :: UTCTime -> [UTCTime]
pastImgsTimes time = let t = utcTimeToPOSIXSeconds time
                         t0 = lastImagePOSIXTime t - pastImgsCount * fromInteger intervalSecs in
                         posixSecondsToUTCTime . (\i -> t0 + i * fromInteger intervalSecs) <$> [1,2..pastImgsCount]

futureImgsTimes :: UTCTime -> [UTCTime]
futureImgsTimes time = let t0 = lastImagePOSIXTime $ utcTimeToPOSIXSeconds time in
                           posixSecondsToUTCTime . (\i -> t0 + i * fromInteger intervalSecs) <$> [1,2..futureImgsCount]

radarImgUrls :: UTCTime -> [Observation Url]
radarImgUrls time = let format = "http://data-4c21db65c81f6.s3.amazonaws.com/eltiempo/maps/\
                                  \%Y/%m/%d/weather/radar/spain/680x537/spain-weather-radar-%Y%m%d%H%M.jpg" in
                     (\t -> Observation t (Just $ formatTime defaultTimeLocale format t)) <$> radarImgTimes time

-- Cache invalidation task
invalidateCacheAfterNextLoop :: ImgCacheState -> IO ()
invalidateCacheAfterNextLoop cacheState = do
  now <- getCurrentTime
  let next = nextImageUTCTime now
  let timeToNext = fromEnum . round $ next `diffUTCTime` now
  let delay = if timeToNext > 0 then timeToNext else fromEnum intervalSecs
  putStrLn $ "Invalidating cache in " ++ show (delay `div` 60) ++ " minutes " ++ show (delay `mod` 60) ++ " seconds"
  threadDelay (10^6 * delay)
  putStrLn "Invalidating cache"
  invalidateAll cacheState
  invalidateCacheAfterNextLoop cacheState
