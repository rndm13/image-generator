{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import qualified System.IO             as SIO
import qualified Data.Text.IO          as TIO
import qualified System.Environment    as E

import qualified Data.List             as L
import qualified Control.Monad         as CM
import qualified System.Process        as P

import           Data.Time.Clock.POSIX

import           Tiles

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "   image-generator gen[R] tile-size width height image1.ppm [image2.ppm ...]"
  putStrLn "   image-generator ext tile-size width-to-add height-to-add image1.ppm"

main :: IO ()
main = do
  args <- E.getArgs
  if (length args < 5) 
  then printUsage >> return ()
  else do
    let mode    = args !! 0
    let sz      = read (args !! 1) 
    let width   = read (args !! 2)
    let height  = read (args !! 3)

    inputImages <- CM.sequence . map (loadImage) . drop 4 $ args

    t <- ((round . (* 1000000)) <$> getPOSIXTime)

    CM.when (take 3 mode == "gen") $ do
      let gen = if | drop 3 mode == "R" -> genImage t (inputImages >>= allRotations) sz width height
                   | otherwise          -> genImage t inputImages sz width height

      handle <- SIO.openFile "generatedImage.ppm" SIO.WriteMode

      TIO.hPutStr handle . showImage $ gen
      SIO.hFlush handle
      SIO.hClose handle
      CM.void . P.runCommand $ "gimp generatedImage.ppm " <> (L.intercalate " " . drop 4 $ args)

    CM.when (mode == "ext") $ do
      let ext = extImage t (head inputImages) sz width height

      handle <- SIO.openFile "generatedImage.ppm" SIO.WriteMode

      TIO.hPutStr handle . showImage $ ext
      SIO.hFlush handle
      SIO.hClose handle

      CM.void . P.runCommand $ "gimp generatedImage.ppm " <> (L.intercalate " " . drop 4 $ args)

    return ()
