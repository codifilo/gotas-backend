module Main where

import ImageUtils
import ElTiempoEs
import Data.Map as M
import Data.Maybe

main :: IO ()
main = do
  result <- readPixelFrom "res/spain-weather-radar-201611121215.jpg" (ImgCoord 0 0)
  case result of
    Left errorMsg -> putStrLn errorMsg
    Right pixel -> print $ pixelToMmh pixel
