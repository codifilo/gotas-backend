{-# LANGUAGE DeriveGeneric #-}
module Provider (GpsCoord (..)
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

precipAt :: Provider -> GpsCoord -> IO [(UTCTime, Maybe Precip)]
precipAt prov coords =
  let imgCoords = locateIn (imgBounds prov) coords in
      if inBounds prov imgCoords
        then do
          time <- getCurrentTime
          let urls = imgUrls prov time
          parallel $ (\u -> urlToPrecip u prov imgCoords) <$> urls
        else return []

inBounds :: Provider -> ImgCoord -> Bool
inBounds p (ImgCoord x y) =
  let bounds = imgBounds p
      w = width bounds
      h = height bounds in
      x >= 0 && x < w && y >= 0 && y < h

urlToPrecip :: (UTCTime, String) -> Provider -> ImgCoord -> IO (UTCTime, Maybe Precip)
urlToPrecip (time, url) prov (ImgCoord x y) = do
  img <- getCachedImg url
  let precip = (\img -> pixelToprecip prov (pixelAt (convertRGB8 img) x y)) <$> img
  return (time, precip)

getCachedImg :: String -> IO (Maybe DynamicImage)
getCachedImg url = do
  bytes <- getCachedBytes url
  let result = bytes >>= decodeImage
  case result of
    Left _    -> return Nothing
    Right img -> return $ Just img

getCachedBytes :: String -> IO (Either String B.ByteString)
getCachedBytes url = do
  let cachedFilePath = "cache" </> takeFileName url
  fileExists <- doesFileExist cachedFilePath
  if fileExists
    then Right <$> B.readFile cachedFilePath
    else do result <- download url
            case result of
              Left e -> do
                let errorMsg = e ++ " url=" ++ url
                _ <- putStrLn errorMsg
                return $ Left errorMsg
              Right bytes -> do
                _ <- B.writeFile cachedFilePath bytes
                return $ Right bytes

download :: String -> IO (Either String B.ByteString)
download url = case parseURI url of
                      Nothing -> return (Left $ "Invalid URL: " ++ url)
                      Just uri -> do
                        result <- tryAny $ simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
                        return $ BF.first (\e -> show e ++ " url: " ++ url) result
