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
import Cache

main :: IO ()
main = do
  cacheState <- newEmptyCacheState
  putStrLn "Starting Server..."
  scotty 3000 $ do
      get "/precip" $ do
          lat <- param "lat"
          lon <- param "lon"
          precip <- liftIO $ precipAt cacheState elTiempoEs (GpsCoord lat lon)
          json precip
