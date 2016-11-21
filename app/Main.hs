module Main where

import Provider
import ElTiempoEs
import Codec.Picture
import qualified Data.ByteString as B
import Network.HTTP
import Network.URI (parseURI, uriPath)
import Data.Time.Clock.POSIX
import Data.Maybe
import System.FilePath
import System.Directory
import Control.Monad

main :: IO ()
main = do
  let pontevedra = GpsCoord 42.4338911 (-8.6568552)
  let sevilla = GpsCoord 38.0186646 (-8.6011768)
  let gijon = GpsCoord 43.5315315 (-5.7384946)
  let santander = GpsCoord 43.4614014 (-3.8461565)
  time <- getPOSIXTime
  let url = head $ imgUrls elTiempoEs time
  result <- getCachedImg url
  case result of
    Left errorMsg -> putStrLn errorMsg
    Right img ->
      case decodeImage img of
        Left errorMsg -> putStrLn errorMsg
        Right img -> print $ amountAt elTiempoEs img pontevedra


getCachedImg :: String -> IO (Either String B.ByteString)
getCachedImg url =
  case parseURI url of
    Nothing -> return (Left ("Invalid URI: " ++ url))
    Just uri ->do
      let cachedFilePath = "cache" </> takeFileName url
      fileExists <- doesFileExist cachedFilePath
      unless fileExists (
          do result <- simpleHTTP (defaultGETRequest_ uri) --TODO: catch errors, timeouts..
             bytes <- getResponseBody result
             B.writeFile cachedFilePath bytes)
      file <- B.readFile cachedFilePath
      return (Right file)
