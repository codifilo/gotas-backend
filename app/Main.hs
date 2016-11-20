module Main where

import Provider
import ElTiempoEs
import Codec.Picture

main :: IO ()
main = do
  let pontevedra = GpsCoord 42.4338911 (-8.6568552)
  let sevilla = GpsCoord 38.0186646 (-8.6011768)
  let gijon = GpsCoord 43.5315315 (-5.7384946)
  let santander = GpsCoord 43.4614014 (-3.8461565)
  result <- readImage "res/spain-weather-radar-201611200800.jpg"
  case result of
    Left errorMsg -> putStrLn errorMsg
    Right img -> print $ amountAt elTiempoEs img sevilla
