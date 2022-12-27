{-# LANGUAGE DerivingStrategies #-}

module Tiles
       ( Pixel(..), newPixel
       , Image
       , Side(..), allSides
       , oppositeSide
       , getSide
       , showMatrix
       , loadImage
       , mergeImageMap
       , rotate
       , splitToTileMap
       , makeLookupMap
       , makeTileMatcher
       , allTiles
       , generateImage
       ) where

import           Data.Char
import           Data.Ord
import qualified Data.List             as L
import qualified Data.List.Split       as LS
import qualified Data.List.Extra       as LE
import qualified Data.Matrix           as M
import           Data.Tuple            (swap)
import qualified Data.Maybe            as Mb
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
          | SBottom 
          deriving stock (Show, Eq)

type Image = M.Matrix Pixel
type Tile = Int
type TileMatcher = Side -> Tile -> Tile -> Bool

instance Show Pixel where
  show (Pixel rp gp bp) = printf "%d %d %d" rp gp bp

instance Read Pixel where
  readsPrec _ input = if isSpace ws1 && isSpace ws2 -- Looks valid 
                      then [(newPixel (read red) (read green) (read blue), rest5)] 
                      else []
    where (red,   rest1) = span isDigit input
          (ws1:   rest2) = rest1
          (green, rest3) = span isDigit rest2
          (ws2:   rest4) = rest3
          (blue,  rest5) = span isDigit rest4

rotate :: M.Matrix a -> M.Matrix a
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

showMatrix :: (Show a) => M.Matrix a -> String
showMatrix m = (printf "P3\n%d %d\n255\n" (M.nrows m) (M.ncols m)) ++ L.intercalate "\n" (map (L.intercalate " " . map (show)) (M.toLists m))

loadImage  :: SIO.FilePath -> IO Image
loadImage file = do
    content <- SIO.readFile file
    let w = read ((words content) !! 1) :: Int
    let numbers = (map (read :: String -> Int)) . (>>= words) . (L.drop 3) . lines $ content
    return . M.fromLists . (LS.chunksOf w) . (map (\[a,b,c] -> newPixel a b c)) . LS.chunksOf 3 $ numbers

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
  | toCollapse        == [] = Nothing
  | length toCollapse == 1  = Just tileMap
  | otherwise = 
    CM.join . 
    L.find Mb.isJust $ 
    map (\e -> M.safeSet [e] indexToCollapse tileMap >>= 
               (collapseWith tileMatcher . updateWith tileMatcher)) toCollapse
    where
      toCollapse = L.minimumBy (comparing lengthFunction) tileMap 
      lengthFunction t = if (length t) > 1 then length t else 9999
      indexToCollapse = Mb.fromJust $ findIndexMatrix ( == toCollapse) tileMap

missingTexture :: Int -> Image
missingTexture size = M.matrix size size (const (Pixel 82 0 100))

generateImage :: Int -> Image -> Int -> Int -> Int -> Image
generateImage seed origImage tileSize width height = 
  mergeImageMap . 
  fmap (\e -> L.singleton . maybe (missingTexture tileSize) fst $ idll LE.!? (head e)) .
  Mb.fromJust . -- M.Matrix [Tile]
  collapseWith matcher . 
  M.mapPos (\_ n -> RS.shuffle' tileSet (length tileSet) (R.mkStdGen (seed + n))) .
  M.matrix width height $ (\(x,y) -> x+y*width)
    where imageMap = splitToTileMap tileSize origImage
          lm = makeLookupMap idMap 
          idll = makeIdLookupList imageMap
          idMap = fmap (\e -> maybe 0 id $ lookup e idll) imageMap
          matcher = makeTileMatcher lm
          tileSet = map (snd) idll

mergeImageMap :: M.Matrix [Image] -> Image
mergeImageMap tm = foldl1 (M.<|>) . map (\row -> foldl1 (M.<->) row) $ tileMap
  where tileMap = L.transpose . M.toLists . fmap (head) $ tm

splitToTileMap :: Int -> Image -> M.Matrix Image
splitToTileMap size origImage = 
  M.fromLists . 
  L.transpose .
  (map (  
    (map M.fromLists) .          
    L.transpose .               
    (map (LS.chunksOf size)))) .
  LS.chunksOf size .   
  M.toLists $ origImage

groupValues :: (Eq k) => [(k,v)] -> [(k, [v])]
groupValues [] = []
groupValues ((curK, curV):rest) = (curK, curV:(map snd sameK)):(groupValues difK)
  where (sameK, difK) = L.partition (\a -> fst a == curK) rest

makeIdLookupList :: M.Matrix Image -> [(Image, Int)]
makeIdLookupList tileMap = 
  (flip zip) [0..] .
  allTiles . 
  makeLookupMap $ tileMap

makeLookupMap :: (Eq a) => M.Matrix a -> [((a,a), [Side])] 
makeLookupMap tm = complete
  where ltor = L.nub $ (M.toLists tileMap) >>= (\a -> zip a (tail a)) 
        utob = L.nub $ (L.transpose . M.toLists $ tileMap) >>= (\a -> zip a (tail a)) 
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
