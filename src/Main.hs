{-# LANGUAGE MultiWayIf, DerivingStrategies #-}

module Main (main) where

import qualified System.IO             as SIO
import qualified Data.Text.IO          as TIO
import qualified System.Environment    as E

import           Text.Printf
import qualified Data.List             as L
import qualified Data.Maybe            as Mb

import qualified Control.Monad         as CM
import qualified System.Process        as P

import           Control.Arrow
import           Data.Time.Clock.POSIX

import           Tiles

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "  image-generator [gen|ext] tile-size width height [OPTIONS] image1.ppm [image2.ppm ...]"
  putStrLn "  Options:"
  putStrLn "    -R or --rotations    Use all rotations of input images as new input."
  putStrLn "    -o=output-file.ppm   Write output to a file instead of stdout"
  putStrLn "    -V or --view=program At the end open inputs and outputs (only when output file is specified) with a program."

data Mode = Generate
          | Extend
          deriving stock (Show, Eq)

data Option = Rotations
            | Unknown
            | View String
            | Output String
            deriving stock (Show, Eq)

isView :: Option -> Bool
isView (View _) = True
isView _        = False

isOutput :: Option -> Bool
isOutput (Output _) = True
isOutput _          = False

prefix :: [String] -> String -> (Bool,String)
prefix prefixes input = maybe (False, "") id . L.find (fst) $ results
  where results = map (\pre -> ((pre==)***id). splitAt (length pre) $ input) prefixes 

readOption :: String -> Option
readOption "-R"          = Rotations
readOption "--rotations" = Rotations
readOption str
  | isV       = View   (tail toView)
  | isO       = Output (tail toOutput)
  | otherwise = Unknown
  where (isV, toView)   = prefix ["-V", "--view"] str
        (isO, toOutput) = prefix ["-o"] str


readArgs :: [String] -> (Mode, Int, Int, Int, [SIO.FilePath], [Option])
readArgs args = (mode, size, width, height, images, readOption <$> options)
  where mode  = if | head args == "gen" -> Generate
                   | head args == "ext" -> Extend
                   | otherwise -> error . printf "unknown mode '%s'" . head $ args
        size    = read (args !! 1)
        width   = read (args !! 2)
        height  = read (args !! 3)
        (images, options) = L.partition ((/='-') . head) . drop 4 $ args

main :: IO ()
main = do
  args <- E.getArgs
  if (length args < 5) 
  then printUsage
  else do
    let (mode, sz, width, height, inputFP, options) = readArgs args

    input <- CM.sequence . map (loadImage) $ inputFP
    let inputImages = if | Rotations `elem` options -> (input >>= allRotations)
                         | otherwise -> input

    t <- ((round . (* 1000000)) <$> getPOSIXTime)

    let out = case mode of {
       Generate -> (genImage t inputImages sz width height);
       Extend   -> (extImage t (head inputImages) sz width height); }

    let outfile = case (L.find (isOutput) options) of {
      Just (Output str) -> str;
      _ -> "";}

    CM.when   (null outfile) $ TIO.putStrLn . showImage $ out
    CM.unless (null outfile) $ saveImage outfile $ out

    let toView  = L.find (isView) options 
    CM.when (Mb.isJust toView) $ do
      let Just (View viewer) = toView 
      CM.void . P.runCommand . L.intercalate " " $ (viewer:outfile:inputFP)
