module Main where
import Codec.Picture
import Data.Word
import Data.Complex
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB

bailoutRadius :: Double
bailoutRadius = 10 ^ 100 --4

exponent' :: Complex Double -> Integer -> Complex Double
exponent' n 1 = n
exponent' n e = (exponent' n (e - 1)) * n

mandelbrot :: Maybe [Complex Double] -> Complex Double -> Integer -> Maybe [Complex Double]
mandelbrot Nothing c 0 = mandelbrot (Just [0 :+ 0]) c 1
mandelbrot list c n = case list of
                           Nothing -> Nothing
                           Just x -> let znOne = (last x)
                                                 in (if ((realPart znOne ^ 2) + ((imagPart znOne ^ 2)) < bailoutRadius) && (n < 1000)
                                                     then mandelbrot (Just $ x ++ [(znOne ^ 2) + c]) c (n + 1)
                                                     else if n < 1000
                                                          then list
                                                          else Nothing)

normalizedIterationCount :: Integer -> Int -> Double -> Complex Double -> Double
normalizedIterationCount p n bigN zn = (fromIntegral n + 1) - (logBase (fromIntegral p) ((log (((realPart zn) ^ 2) + ((imagPart zn) ^ 2)) / 2) / log(bigN)))

mandelbrotSet :: Complex Double -> Maybe Double
mandelbrotSet c = let mandelbrotV = (mandelbrot Nothing c 0)
                                    in case mandelbrotV of
                                            Nothing -> Nothing
                                            Just x -> Just $ normalizedIterationCount 2 (length x) bailoutRadius (last x)

complex :: Double -> Double -> Int -> Int -> [[Complex Double]]
complex width height resWidth resHeight = [[x :+ y
                                           | x <- [0 - halfWidth,  (0 - halfWidth)  + stepWidth  .. halfWidth  - stepWidth]]
                                           | y <- [0 - halfHeight, (0 - halfHeight) + stepHeight .. halfHeight - stepHeight]]
                                          where
                                            stepWidth  = (width  / fromIntegral resWidth)
                                            stepHeight = (height / fromIntegral resHeight)
                                            halfWidth  = (width / 2)
                                            halfHeight = (height / 2)

colorize :: Maybe Double -> PixelRGB8
colorize val = case val of
                 Nothing -> PixelRGB8 0 0 0
                 Just pix -> let rgb = hsl (fromIntegral ((round pix) `mod` 360)) 0.5 0.5 :: RGB Double
                                 convert x = fromIntegral (floor (x * 255)) :: Word8
                                 in PixelRGB8 (convert $ channelRed rgb) (convert $ channelGreen rgb) (convert $ channelBlue rgb)

imageTest :: Double -> Double -> Int -> Int -> DynamicImage
imageTest width height wres hres = let image = map mandelbrotSet (concat (complex width height wres hres))
                                       boring x y = colorize (image !! ((y * wres) + x))
                                   in (ImageRGB8 $ generateImage boring wres hres)

main :: IO ()
main = do
       saveBmpImage "test.bmp" (imageTest 5 2.5 2048 1024)
