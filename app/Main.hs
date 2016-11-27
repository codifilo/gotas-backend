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
import Control.Concurrent
import Cache

main :: IO ()
main = do
  cacheState <- newEmptyCacheState
  forkIO $ cleanTask 30 cacheState
  putStrLn "Starting Server..."
  scotty 3000 $
      get "/precip" $ do
          lat <- param "lat"
          lon <- param "lon"
          precip <- liftIO $ precipAt cacheState elTiempoEs (GpsCoord lat lon)
          json precip

cleanTask :: Int -> ImgCacheState -> IO ()
cleanTask expirySnds cacheState = do
  threadDelay (10^6 * expirySnds)
  putStrLn "Cleaning old values"
  cleanOldValues (fromIntegral expirySnds) cacheState
  cleanTask expirySnds cacheState
