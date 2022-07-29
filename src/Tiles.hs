{-# LANGUAGE DerivingStrategies #-}

module Tiles
       ( Pixel(..), newPixel
       , Image
       , Side(..)
       , oppositeSide
       , getSide
       , matchTiles
       , showMatrix
       , loadImage
       ) where

import           Data.Char
import qualified Data.List       as L
import qualified Data.List.Split as LS
import qualified Data.Matrix     as M
import qualified Data.Vector     as V

import qualified System.IO       as SIO

import           Text.Printf

data Pixel = Pixel { r :: Int
                   , g :: Int
                   , b :: Int
                   } deriving stock Eq

newPixel :: Int -> Int -> Int -> Pixel
newPixel red green blue = Pixel {r = red, g = green, b = blue}

instance Show Pixel where
  show p = printf "%d %d %d" (r p) (g p) (b p)

instance Read Pixel where
  readsPrec _ input = if ws1 == ' ' && ws2 == ' ' -- Looks valid
                      then [(newPixel (read red) (read green) (read blue), rest5)]
                      else []
      where
    (red,   rest1) = span isDigit input
    (ws1:   rest2) = rest1
    (green, rest3) = span isDigit rest2
    (ws2:   rest4) = rest3
    (blue,  rest5) = span isDigit rest4

data Side = SRight
          | SLeft
          | STop
          | SBottom

type Image = M.Matrix Pixel

getSide :: Side -> Image -> V.Vector Pixel
getSide STop    m = M.getRow 1 m
getSide SBottom m = M.getRow (M.nrows m) m
getSide SLeft   m = M.getCol 1 m
getSide SRight  m = M.getCol (M.ncols m) m

oppositeSide :: Side -> Side
oppositeSide STop    = SBottom
oppositeSide SBottom = STop
oppositeSide SLeft   = SRight
oppositeSide SRight  = SLeft

matchTiles :: Side -> Image -> Image -> Bool
matchTiles s im1 im2 = (getSide s im1) == (getSide (oppositeSide s) im2)

showMatrix :: (Show a) => M.Matrix a -> String
showMatrix m = (printf "P3\n%d %d\n255\n" (M.nrows m) (M.ncols m)) ++ L.intercalate "\n" (map (L.intercalate " " . map (show)) (M.toLists m))

loadImage  :: SIO.FilePath -> IO Image
loadImage file = do
    content <- SIO.readFile file
    let w = read ((words content) !! 1) :: Int
    let h = read ((words content) !! 2) :: Int
    let numbers = (map (read :: String -> Int)) . words . (L.intercalate "\n") . (L.drop 3) . lines $ content
    return . M.fromLists . (LS.chunksOf w) . (map (\[a,b,c] -> newPixel a b c)) . LS.chunksOf 3 $ numbers
