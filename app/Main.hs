module Main where

import Codec.Picture
import Codec.Picture.Types
import ImgLocation
import Data.Foldable

main :: IO ()
main = do
  pixel <- pixelAtIMg "res/spain-weather-radar-201611120000.jpg" (ImgCoord 0 0)
  forM_ pixel print

pixelAtIMg :: FilePath -> ImgCoord -> IO (Maybe PixelRGB8)
pixelAtIMg path (ImgCoord x y) = do
  result <- readImage path
  case result of
    Left s -> putStrLn s >> return Nothing
    Right img -> return $ pixelRGB8At img x y


pixelYCbCr8At :: DynamicImage -> Int -> Int -> Maybe PixelYCbCr8
pixelYCbCr8At (ImageYCbCr8 img@(Image w h _)) x y
  | between x 0 w && between y 0 h  = Just $ pixelAt img x y
  | otherwise       = Nothing
pixelYCbCr8At _ _ _ = Nothing

between i min max = i >= min && i < max

pixelRGB8At :: DynamicImage -> Int -> Int -> Maybe PixelRGB8
pixelRGB8At img x y = toRGB <$> pixelYCbCr8At img x y

toRGB :: PixelYCbCr8 -> PixelRGB8
toRGB = convertPixel
