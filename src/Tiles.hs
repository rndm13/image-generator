{-# LANGUAGE DerivingStrategies #-}

module Tiles
       ( Pixel(..)
       , Image
       , Side(..), allSides
       , showMatrix
       , loadImage
       , splitImageMap
       , allTiles
       , genImageM
       , genImage
       , makeLookupMap
       , mergeImageMap
       , allRotations
       ) where

import           Data.Char
import           Data.Ord
import qualified Data.List             as L
import qualified Data.List.Split       as LS
import qualified Data.List.Extra       as LE
import qualified Data.Matrix           as M
import           Data.Tuple            (swap)
import qualified Data.Maybe            as Mb

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
          | SBottom 
          deriving stock (Show, Eq)

type Image = M.Matrix Pixel
type Tile = Int
type TileMatcher = Side -> Tile -> Tile -> Bool

instance Show Pixel where
  show (Pixel rp gp bp) = printf "%d %d %d" rp gp bp

instance Read Pixel where
  readsPrec _ input = if isSpace ws1 && isSpace ws2 -- Looks valid 
                      then [(Pixel (read red) (read green) (read blue), rest5)] 
                      else []
    where (red,   rest1) = span isDigit input
          (ws1:   rest2) = rest1
          (green, rest3) = span isDigit rest2
          (ws2:   rest4) = rest3
          (blue,  rest5) = span isDigit rest4

rotate :: M.Matrix a -> M.Matrix a
rotate im = M.fromLists . L.transpose . (map reverse) . M.toLists $ im

showMatrix :: (Show a) => M.Matrix a -> String
showMatrix m = (printf "P3\n%d %d\n255\n" (M.ncols m) (M.nrows m)) ++ L.intercalate "\n" (map (L.intercalate " " . map (show)) (M.toLists m))

loadImage  :: SIO.FilePath -> IO Image
loadImage file = do
    content <- L.intercalate "\n" . filter ((/='#') . head) . lines <$> SIO.readFile file
    let w = read ((words content) !! 1) :: Int
    let numbers = (map (read :: String -> Int)) . (>>= words) . (L.drop 3) . lines $ content
    return . M.fromLists . (LS.chunksOf w) . (map (\[a,b,c] -> Pixel a b c)) . LS.chunksOf 3 $ numbers

findIndexMatrix :: (a -> Bool) -> M.Matrix a -> Maybe (Int,Int)
findIndexMatrix f matr = CM.join . (L.find Mb.isJust) . M.toList $ M.mapPos func matr
    where
  func = (\coord a -> if f a then Just coord else Nothing)

move :: Side -> (Int, Int) -> (Int, Int)
move STop    (x, y) = (x,y - 1)
move SBottom (x, y) = (x,y + 1)
move SLeft   (x, y) = (x - 1,y)
move SRight  (x, y) = (x + 1,y)

allSides :: [Side]
allSides = [STop, SRight, SBottom, SLeft]

adjacentTiles :: M.Matrix a -> (Int,Int) -> [(Side,a)]
adjacentTiles tileMap coord = 
  map (\(mt,s) -> (s,Mb.fromJust mt)) .
  filter (\(t,_) -> Mb.isJust t) .
  zip (
    map (\(x,y) -> M.safeGet x y tileMap) . 
    map (\s -> move s coord) $ allSides) $ allSides

updateWith :: TileMatcher -> M.Matrix [Tile] -> M.Matrix [Tile]
updateWith tileMatcher tileMap
  | result == tileMap = result
  | otherwise         = updateWith tileMatcher result
    where
  doesTileFit (x,y) tile = all (\(side, possibleTiles) -> any (tileMatcher side tile) possibleTiles) (adjacentTiles tileMap (x,y))
  result        = M.mapPos (\(x,y) tiles -> filter (doesTileFit (x,y)) tiles) tileMap

collapseWith :: TileMatcher -> M.Matrix [Tile] -> Maybe (M.Matrix [Tile])
collapseWith tileMatcher tileMap
  | null toCollapse         = Nothing
  | length toCollapse == 1  = Just tileMap
  | otherwise = 
    CM.join . 
    L.find Mb.isJust $ 
    map (\e -> (M.safeSet [e] indexToCollapse tileMap) >>= (collapseWith tileMatcher . updateWith tileMatcher)) toCollapse
    where
      lengthFunction t = (\a -> if a <= 1 then 9999 else a) . length $ t
      indexToCollapse  = Mb.fromJust $ findIndexMatrix ( == toCollapse) tileMap
      toCollapse       = L.minimumBy (comparing lengthFunction) tileMap 

missingTexture :: Int -> Image
missingTexture size = M.matrix size size (const (Pixel 82 0 100))

allRotations :: Image -> [Image]
allRotations origImage = take 4 . iterate rotate $ origImage

genImageM :: Int -> [Image] -> Int -> Int -> Int -> Image
genImageM seed origImage tileSize width height = 
  mergeImageMap . 
  fmap (\e -> L.singleton . maybe (missingTexture tileSize) fst $ idll LE.!? (head e)) .
  Mb.fromJust . 
  collapseWith matcher . 
  M.mapPos (\_ n -> RS.shuffle' tileSet (length tileSet) (R.mkStdGen (seed + n))) .
  M.matrix width height $ (\(x,y) -> x+y*width)
    where imageMap = map (splitImageMap tileSize) $ origImage
          lm      = idMap >>= makeLookupMap 
          idll    = imageMap >>= makeIdLookupList
          idMap   = map (fmap (\e -> maybe (-1) id $ lookup e idll)) imageMap
          matcher = makeTileMatcher lm
          tileSet = map (snd) idll

genImage :: Int -> Image -> Int -> Int -> Int -> Image
genImage seed origImage tileSize width height = 
  mergeImageMap . 
  fmap (\e -> L.singleton . maybe (missingTexture tileSize) fst $ idll LE.!? (head e)) .
  Mb.fromJust . 
  collapseWith matcher . 
  M.mapPos (\_ n -> RS.shuffle' tileSet (length tileSet) (R.mkStdGen (seed + n))) .
  M.matrix width height $ (\(x,y) -> x+y*width)
    where imageMap = splitImageMap tileSize origImage
          lm      = makeLookupMap idMap
          idll    = makeIdLookupList imageMap 
          idMap   = fmap (\e -> maybe (-1) id $ lookup e idll) imageMap
          matcher = makeTileMatcher lm
          tileSet = map (snd) idll

mergeImageMap :: M.Matrix [Image] -> Image
mergeImageMap tm = foldl1 (M.<|>) . map (foldl1 (M.<->)) $ tileMap
  where tileMap = M.toLists . fmap (head) $ tm

splitImageMap :: Int -> Image -> M.Matrix Image
splitImageMap size origImage = 
  M.fromLists . 
  map 
    (map M.fromLists . 
    L.transpose .
    map (LS.chunksOf size)) .
  LS.chunksOf size .
  M.toLists $ origImage

groupValues :: (Eq k, Eq v) => [(k,v)] -> [(k, [v])]
groupValues [] = []
groupValues ((curK, curV):rest) = (curK, L.nub $ curV:(map snd sameK)):(groupValues difK)
  where (sameK, difK) = L.partition (\a -> fst a == curK) rest

makeIdLookupList :: M.Matrix Image -> [(Image, Int)]
makeIdLookupList tileMap = 
  (flip zip) [0..] .
  allTiles . 
  makeLookupMap $ tileMap

makeLookupMap :: (Eq a) => M.Matrix a -> [((a,a), [Side])] 
makeLookupMap tm = complete
  where ltor = L.nub $ (M.toLists tileMap) >>= (\a -> zip a (tail a)) 
        utob = L.nub $ (M.toLists . M.transpose $ tileMap) >>= (\a -> zip a (tail a)) 
        complete = 
          groupValues $
          (zip ltor (repeat SRight)) <>
          (zip (map (swap) ltor) (repeat SLeft)) <>
          (zip utob (repeat SBottom)) <>
          (zip (map (swap) utob) (repeat STop))
        tileMap = tm

allTiles :: (Eq a) => [((a,a), [Side])] -> [a]
allTiles input = L.nub (input >>= ((\a -> [fst a, snd a]) . fst))

makeTileMatcher :: [((Tile, Tile), [Side])] -> TileMatcher
makeTileMatcher lookupList = (\side im1 im2 -> elem side (mtrx M.! ((im1 + 1), (im2 + 1))))
  where size = length . allTiles $ lookupList
        mtrx = M.matrix size size (\(x,y) -> maybe [] id $ lookup ((x - 1), (y - 1)) lookupList) 
