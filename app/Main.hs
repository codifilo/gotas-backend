{-# LANGUAGE OverloadedStrings #-}
module Main where

import Provider
import ElTiempoEs
import Web.Scotty
import Data.Text.Lazy
import Data.Text.Read
import Data.Text.Internal.Lazy
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock.POSIX
import Cache
import Control.Concurrent

main :: IO ()
main = do
  cacheState <- newEmptyCacheState
  forkIO $ cacheInvalidationLoop provider cacheState
  putStrLn "Starting Server..."
  scotty 3000 $
      get "/precip" $ do
          lat <- param "lat"
          lon <- param "lon"
          precip <- liftIO $ precipAt cacheState provider (GpsCoord lat lon)
          json precip

provider :: Provider
provider = elTiempoEs
