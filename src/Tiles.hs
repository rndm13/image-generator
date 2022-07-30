{-# LANGUAGE DerivingStrategies #-}

module Tiles
       ( Pixel(..), newPixel
       , Image
       , Side(..), allSides
       , oppositeSide
       , getSide
       , matchTiles
       , showMatrix
       , loadImage
       , generateTileMap
       , mergeTileMap
       , rotate
       ) where

import           Data.Char
import qualified Data.List             as L
import qualified Data.List.Extra       as LE
import qualified Data.List.Split       as LS
import qualified Data.Matrix           as M
import qualified Data.Maybe            as Mb
import qualified Data.Tuple            as T
import qualified Data.Vector           as V

import qualified Control.Monad         as CM

import qualified System.IO             as SIO

import qualified System.Random         as R
import qualified System.Random.Shuffle as RS

import           Text.Printf

data Pixel = Pixel { r :: Int
                   , g :: Int
                   , b :: Int
                   } deriving stock Eq

data Side = SRight
          | SLeft
          | STop
          | SBottom deriving stock Show

type Image = M.Matrix Pixel

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


rotate :: Image -> Image
rotate im = M.fromLists . L.transpose . (map reverse) . L.transpose . M.toLists . M.transpose $ im

newPixel :: Int -> Int -> Int -> Pixel
newPixel red green blue = Pixel {r = red, g = green, b = blue}

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

findIndexMatrix :: (a -> Bool) -> M.Matrix a -> Maybe (Int,Int)
findIndexMatrix f matr = CM.join $ L.find Mb.isJust $ M.toList $ M.mapPos func matr
    where
  func = (\coord a -> if f a then Just coord else Nothing)

move :: Side -> (Int,Int) -> (Int,Int)
move STop    (x, y) = (x,y - 1)
move SBottom (x, y) = (x,y + 1)
move SLeft   (x, y) = (x - 1,y)
move SRight  (x, y) = (x + 1,y)

allSides :: [Side]
allSides = [STop,SRight,SBottom, SLeft]

adjacentTiles :: M.Matrix [Image] -> (Int,Int) -> [(Side,[Image])]
adjacentTiles tileMap = (\coord -> map (\(mt,s) -> (s,Mb.fromJust mt)) $ filter (\(t,_) -> Mb.isJust t) $ zip (map (\(x,y) -> M.safeGet x y tileMap) $ map (\s -> move s coord) allSides) allSides)

update :: M.Matrix [Image] -> M.Matrix [Image]
update tileMap
  | result == tileMap = result
  | otherwise         = update result
    where
  doesTileFit   = (\(x,y) tile -> all (\(side, possibleTiles) -> any (matchTiles side tile) possibleTiles) (adjacentTiles tileMap (x,y)))
  -- (Int,Int) -> Image -> Bool
  result        = M.mapPos (\(x,y) tiles -> filter (doesTileFit (x,y)) tiles) tileMap

collapse :: M.Matrix [Image] -> Maybe (M.Matrix [Image])
collapse tileMap
  | toCollapse == []       = Nothing
  | length toCollapse == 1 = Just tileMap
  | otherwise = CM.join $ L.find Mb.isJust $ map (\e -> maybe Nothing (collapse . update) $ M.safeSet [e] indexToCollapse tileMap) toCollapse
    where
      toCollapse = LE.minimumOn lengthFunction $ map (LE.minimumOn lengthFunction) $ M.toLists tileMap
      lengthFunction = (\t -> if (length t) > 1 then length t else 9999)
      indexToCollapse = Mb.fromJust $ findIndexMatrix ( == toCollapse) tileMap

generateTileMap :: Int -> [Image] -> Int -> Int -> M.Matrix [Image]
generateTileMap seed tileSet width height = Mb.fromJust $ collapse $ M.mapPos (\_ n -> RS.shuffle' tileSet (length tileSet) (R.mkStdGen (seed + n))) $ M.matrix width height (\(x,y) -> x+y*width)

mergeTileMap :: M.Matrix [Image] -> Image
mergeTileMap tm = (\col -> foldl (M.<->) (head col) (tail col)) $ map (\row -> foldl (M.<|>) (head row) (tail row)) tileMap
    where
  tileMap = L.transpose $ map (map (head)) $ M.toLists tm
