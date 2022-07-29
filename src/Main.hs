module Main (main) where
 
import qualified System.IO      as SIO
import qualified System.Process as P
import qualified System.Random  as R

import qualified Data.Matrix    as M

import           Tiles

main :: IO ()
main = do 
    im1 <- loadImage "/home/rndm/programming/haskell/image-generator/tiles/test1.ppm"
    putStrLn . showMatrix $ im1
    print $ matchTiles STop im1 im1
    print $ matchTiles SBottom im1 im1
    print $ matchTiles SLeft im1 im1
    print $ matchTiles SRight im1 im1
