{-# LANGUAGE OverloadedStrings #-}
module Main where

import Provider
import ElTiempoEs
import Web.Scotty
import Control.Monad.Trans
import Cache
import Control.Concurrent

main :: IO ()
main = do
  let provider = elTiempoEs
  cacheState <- newEmptyCacheState
  forkIO $ cacheInvalidationLoop provider cacheState
  scotty 3000 $
      get "/precip" $ do
          lat <- param "lat"
          lon <- param "lon"
          precip <- liftIO $ precipAt cacheState provider (GpsCoord lat lon)
          json precip
