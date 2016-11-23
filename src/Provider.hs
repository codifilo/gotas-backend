module Provider (GpsCoord (..)
                 , ImgBounds (..)
                 , Provider (..)
                 , toRadians
                 , precipitationAt) where

import Codec.Picture
import Data.Map as M
import Data.Time.Clock.POSIX
import qualified Data.ByteString as B
import System.FilePath
import System.Directory
import Network.HTTP
import Network.URI (parseURI, uriPath, URI)
import Control.Exception.Enclosed
import GHC.Exception
import Data.Bifunctor
import Data.Either
import Data.Traversable
import Control.Concurrent.ParallelIO

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

precipitationAt :: Provider -> GpsCoord -> IO [Float]
precipitationAt prov coords = do
  time <- getPOSIXTime
  let urls = imgUrls prov time
  downloads <- parallel $ getCachedImg <$> urls
  let imgs = downloads >>= either (const []) (: [])
  let (ImgCoord x y) = locateIn (imgBounds prov) coords
  return $ (\img -> pixelToAmount prov (pixelAt (convertRGB8 img) x y)) <$> imgs

getCachedImg :: String -> IO (Either String DynamicImage)
getCachedImg url = do
  bytes <- getCachedBytes url
  return $ bytes >>= decodeImage

getCachedBytes :: String -> IO (Either String B.ByteString)
getCachedBytes url = do
  let cachedFilePath = "cache" </> takeFileName url
  fileExists <- doesFileExist cachedFilePath
  if fileExists
    then do
      _ <- putStrLn $ "Read from cache " ++ cachedFilePath
      Right <$> B.readFile cachedFilePath
    else do
            _ <- putStrLn $ "Downloading " ++ url
            result <- tryGetBytes url
            case result of
              Left e -> do
                let errorMsg = e ++ " url=" ++ url
                _ <- putStrLn e
                return $ Left errorMsg
              Right bytes -> do
                _ <- putStrLn $ "Img downloaded to: " ++ cachedFilePath
                _ <- B.writeFile cachedFilePath bytes
                return $ Right bytes

tryGetBytes :: String -> IO (Either String B.ByteString)
tryGetBytes url = case parseURI url of
                      Nothing -> return (Left $ "Invalid URL: " ++ url)
                      Just uri -> do
                        result <- tryAny $ simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
                        return $ first (\e -> show e ++ " url: " ++ url) result
