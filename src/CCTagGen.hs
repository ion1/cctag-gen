{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module CCTagGen
( Svg
, renderSvgFile, renderSvg
, ccTag3, ccTag4
) where

import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Text.Blaze.Svg.Renderer.Utf8
import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11.Attributes as A
import System.IO

renderSvgFile :: FilePath -> Svg -> IO ()
renderSvgFile fp svg =
  withFile fp WriteMode $ \h ->
    BSL.hPutStr h (renderSvg svg)

ccTag3, ccTag4 :: Int -> Svg
ccTag3 = ccTag . ccTag3Radiuses
ccTag4 = ccTag . ccTag4Radiuses

ccTag :: [Rational] -> Svg
ccTag rs = docTypeSvg ! A.version "1.1"
         ! A.width "210mm" ! A.height "210mm"
         ! A.viewbox "-105 -105 210 210"
         $ sequence_ circles
  where
    circles = zipWith (!)
      (map (\r -> circle ! A.r (toValue @Double . realToFrac . (75 *) $ r)) rs)
      colors
    colors = A.fill "black" : A.fill "white" : colors

ccTag3Radiuses, ccTag4Radiuses :: Int -> [Rational]
ccTag3Radiuses = ccTagRadiuses 5 0.10 0.15
ccTag4Radiuses = ccTagRadiuses 7 0.08 0.12

ccTagRadiuses :: Int       -- ^ Number of bits in the identifier
              -> Rational  -- ^ Size step for a zero
              -> Rational  -- ^ Size step for a one
              -> Int       -- ^ Identifier
              -> [Rational]
ccTagRadiuses nBits step0 step1 n = radiuses
  where
    radiuses = scanl (-) 1 steps
    steps = map (\bit -> if bit then step1 else step0) bits
    bits  = map (testBit (n - 1)) [nBits - 1, nBits - 2 .. 0]
