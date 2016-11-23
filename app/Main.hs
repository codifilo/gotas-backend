module Main where

import Provider
import ElTiempoEs

main :: IO ()
main = do
  let pontevedra = GpsCoord 42.4338911 (-8.6568552)
  let sevilla = GpsCoord 38.0186646 (-8.6011768)
  let gijon = GpsCoord 43.5315315 (-5.7384946)
  let santander = GpsCoord 43.4614014 (-3.8461565)
  precipitations <- precipitationAt elTiempoEs gijon
  print precipitations
