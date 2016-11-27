{-# LANGUAGE DeriveGeneric #-}
module Provider (ImgCacheState
                 , GpsCoord (..)
                 , ImgBounds (..)
                 , Provider (..)
                 , Precip (..)
                 , toRadians
                 , precipAt) where

import Codec.Picture
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.ByteString as B
import System.FilePath
import System.Directory
import Network.HTTP
import Network.URI (parseURI, uriPath, URI)
import Control.Exception.Enclosed
import qualified Data.Bifunctor as BF
import Control.Concurrent.ParallelIO
import qualified Control.Arrow as A
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Cache

type ImgCacheState = CacheState String DynamicImage

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
  , pixelToprecip :: PixelRGB8 -> Precip
  , imgUrls       :: UTCTime -> [(UTCTime, String)]
}

data Precip = Rain Float
            | Snow Float
            | Mixed Float
    deriving (Show, Generic)

instance ToJSON Precip
instance FromJSON Precip

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

precipAt :: ImgCacheState ->  Provider -> GpsCoord -> IO [(UTCTime, Maybe Precip)]
precipAt cacheState prov coords =
  let imgCoords = locateIn (imgBounds prov) coords in
      if inBounds prov imgCoords
        then do
          time <- getCurrentTime
          let urls = imgUrls prov time
          parallel $ (\u -> urlToPrecip cacheState u prov imgCoords) <$> urls
        else return []

inBounds :: Provider -> ImgCoord -> Bool
inBounds p (ImgCoord x y) =
  let bounds = imgBounds p
      w = width bounds
      h = height bounds in
      x >= 0 && x < w && y >= 0 && y < h

urlToPrecip :: ImgCacheState -> (UTCTime, String) -> Provider -> ImgCoord -> IO (UTCTime, Maybe Precip)
urlToPrecip cacheState (time, url) prov (ImgCoord x y) = do
  img <- readValue cacheState url downloadImg
  let precip = (\img -> pixelToprecip prov (pixelAt (convertRGB8 img) x y)) <$> img
  return (time, precip)

downloadImg :: String -> IO (Maybe DynamicImage)
downloadImg url = case parseURI url of
                      Nothing -> do
                        putStrLn $ "Invalid URL: " ++ url
                        return Nothing
                      Just uri -> do
                        response <- tryAny $ simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
                        case response of
                          Left e -> do
                            putStrLn $ show e ++ " url: " ++ url
                            return Nothing
                          Right bytes ->
                            case decodeImage bytes of
                              Left e -> do
                                putStrLn $ show e ++ " url: " ++ url
                                return Nothing
                              Right img -> return $ Just img
