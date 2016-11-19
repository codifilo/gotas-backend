module Main where

import ImageUtils
import Data.Foldable

main :: IO ()
main = do
  pixel <- readPixelFrom "res/spain-weather-radar-201611120000.jpg" (ImgCoord 0 0)
  forM_ pixel print
