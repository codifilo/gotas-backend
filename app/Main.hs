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

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
      get "/precip" $ do
          lat <- param "lat"
          lon <- param "lon"
          precip <- liftIO $ precipAt elTiempoEs (GpsCoord lat lon)
          json precip
