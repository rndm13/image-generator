module Main (main) where

import qualified System.IO             as SIO
import qualified System.Process        as P
import qualified System.Environment    as E

import qualified Control.Monad         as CM

import           Data.Time.Clock.POSIX

import           Tiles

main :: IO ()
main = do
  args <- E.getArgs

  inputImage <- loadImage (args !! 0)
  let sz      = read (args !! 1) 
  let width   = read (args !! 2)
  let height  = read (args !! 3)

  t <- ((round . (* 1000000)) <$> getPOSIXTime)
  
  let lm = makeLookupMap . splitToTileMap sz $ inputImage 
  print . length . allTiles $ lm

  SIO.writeFile "generatedImage.ppm" . showMatrix $ generateImage t inputImage sz width height
  CM.void . P.runCommand $ "gimp generatedImage.ppm " -- <> (args !! 0)
