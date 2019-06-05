module Main (main) where

import Control.Monad
import System.Directory
import Text.Printf

import CCTagGen

main :: IO ()
main = do
  createDirectoryIfMissing False "output"
  forM_ [1..32] $ \n -> renderSvgFile (printf "output/cctag3-%02d.svg" n) (ccTag3 n)
  forM_ [1..128] $ \n -> renderSvgFile (printf "output/cctag4-%03d.svg" n) (ccTag4 n)
