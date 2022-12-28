{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import qualified System.IO             as SIO
import qualified System.Environment    as E

import qualified Data.List             as L
import qualified Control.Monad         as CM
import qualified System.Process        as P

import           Data.Time.Clock.POSIX

import           Tiles

main :: IO ()
main = do
  args <- E.getArgs

  let sz      = read (args !! 0) 
  let width   = read (args !! 1)
  let height  = read (args !! 2)

  inputImages <- CM.sequence . map (loadImage) . drop 3 $ args

  t <- ((round . (* 1000000)) <$> getPOSIXTime)

  let gen = if | length inputImages > 1 -> genImageM t inputImages sz width height
               | otherwise              -> genImage  t (head inputImages) sz width height

  SIO.writeFile "generatedImage.ppm" . showMatrix $ gen
 
  CM.void . P.runCommand $ "gimp generatedImage.ppm " <> (L.intercalate " " . drop 3 $ args)
