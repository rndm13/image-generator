{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}

module Tiles
       ( Pixel(..)
       , Image
       , Side(..), allSides
       , showImage
       , loadImage
       , saveImage
       , splitImageMap
       , allTiles
       , genImage
       , extImage
       , makeLookupMap
       , mergeImageMap
       , allRotations
       ) where

import qualified Data.Text             as T
import qualified Data.Text.Read        as TR
import qualified Data.List             as L
import qualified Data.List.Split       as LS
import qualified Data.List.Extra       as LE
import qualified Data.Matrix           as M
import           Data.Tuple            (swap)
import qualified Data.Maybe            as Mb

import qualified Data.Text.IO          as TIO
import qualified System.IO             as SIO

import qualified System.Random         as R
import qualified System.Random.Shuffle as RS

import           Text.Printf

data Pixel = Pixel 
  { r :: Int
  , g :: Int
  , b :: Int
  } deriving stock Eq

data Side = SRight
          | SLeft
          | STop
          | SBottom 
          deriving stock (Show, Eq)

type Image = M.Matrix Pixel
type ImageId = Int
type TileMatcher = M.Matrix [Side]

showPixel :: Pixel -> T.Text
showPixel (Pixel rp gp bp) = T.pack . printf "%d %d %d\n" rp gp $ bp

showImage :: Image -> T.Text
showImage m = 
  T.append 
    (T.pack $ printf "P3\n%d %d\n255\n" (M.ncols m) (M.nrows m))
    (T.unlines (map (T.unwords) . M.toLists . fmap showPixel $ m))

rotate :: M.Matrix a -> M.Matrix a
rotate im = M.transpose . M.fromLists . (map reverse) . M.toLists $ im

fromRight :: Either a b -> b
fromRight (Right v) = v
fromRight (Left  _) = error "fromRight call on Left value"

loadImage  :: SIO.FilePath -> IO Image
loadImage file = do
    content <- (filter ((/='#') . T.head) . T.lines <$> TIO.readFile file) :: IO [T.Text]
    let (w, _) = fromRight . TR.decimal $ (content !! 1)
    let numbers = fmap (fst . fromRight . TR.decimal) . (drop 3) $ content
    return . M.fromLists . (LS.chunksOf w) . (map (\[a,b,c] -> Pixel a b c)) . LS.chunksOf 3 $! numbers

saveImage :: SIO.FilePath -> Image -> IO ()
saveImage fp = TIO.writeFile fp . showImage

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
  zip (map (\(x,y) -> M.safeGet x y tileMap) . 
       map (\s -> move s coord) $ allSides) $ allSides

updateWith :: TileMatcher -> M.Matrix [ImageId] -> M.Matrix [ImageId]
updateWith tileMatcher tileMap
  | result == tileMap = result
  | otherwise         = updateWith tileMatcher result
    where doesTileFit (x,y) tile = all (\(side, possibleTiles) -> any (match tileMatcher side tile) possibleTiles) (adjacentTiles tileMap (x,y))
          result                 = M.mapPos (\p -> filter (doesTileFit p)) tileMap

-- The ugly part
getMin0Matrix' :: (a -> Int) -> [(a, (Int,Int))] -> (a, (Int, Int))
getMin0Matrix' _ [] = error "matrix should always have an element"
getMin0Matrix' _ (cur:[]) = cur
getMin0Matrix' func (cur:rest) 
  | (func . fst $ cur) == 0 = cur
  | ((func . fst $ restMin0Matrix) < (func . fst $ cur)) = restMin0Matrix 
  | otherwise = cur
  where restMin0Matrix = getMin0Matrix' func rest

getMin0Matrix :: (a -> Int) -> M.Matrix a -> (a, (Int,Int))
getMin0Matrix func matr = getMin0Matrix' func list
  where list = zip (M.toList $ matr) [(x,y) | x <- [1..M.ncols matr], y <- [1..M.nrows matr]]

mapFirstJust :: (a -> Maybe b) -> [a] -> Maybe b
mapFirstJust _ [] = Nothing
mapFirstJust func (e:rest)
  | Mb.isJust . func $ e = func e
  | otherwise = mapFirstJust func rest

collapseWith :: TileMatcher -> M.Matrix [ImageId] -> Maybe (M.Matrix [ImageId])
collapseWith tileMatcher tileMap
  | null toCollapse          = Nothing
  | null . tail $ toCollapse = Just tileMap
  | otherwise = 
    mapFirstJust (\e -> (M.safeSet [e] indexToCollapse tileMap) >>= (collapseWith tileMatcher . updateWith tileMatcher)) toCollapse
    where lengthFunction t = (\a -> if a == 1 then 9999 else a) . length $ t
          (toCollapse, indexToCollapse) = getMin0Matrix lengthFunction tileMap

missingTexture :: Int -> Image
missingTexture size = M.matrix size size (const (Pixel 82 0 100))

allRotations :: M.Matrix a -> [M.Matrix a]
allRotations origImage = take 4 . iterate rotate $ origImage

genImage :: Int -> [Image] -> Int -> Int -> Int -> Image
genImage seed origImage tileSize width height = 
  mergeImageMap . 
  fmap (\e -> L.singleton . maybe (missingTexture tileSize) fst $ idll LE.!? (head e)) .
  Mb.fromJust . 
  collapseWith matcher . 
  M.matrix width height $ (\(x, y) -> RS.shuffle' tileSet (length tileSet) (genFn x y)) 
    where imageMap = map (splitImageMap tileSize) origImage
          lm       = (idMap >>= makeLookupMap) :: [((ImageId, ImageId), [Side])]
          idll     = makeIdLookupList $ imageMap >>= M.toList
          idMap    = map (fmap (\e -> maybe (-1) id . lookup e $ idll)) imageMap
          matcher  = makeTileMatcher lm
          tileSet  = map (snd) idll 
          genFn x y = R.mkStdGen (seed + (x + y * width))

extImage :: Int -> Image -> Int -> Int -> Int -> Image
extImage seed origImage tileSize width height =
  mergeImageMap . 
  fmap (\e -> L.singleton . maybe (missingTexture tileSize) fst $ idll LE.!? (head e)) .
  Mb.fromJust . 
  collapseWith matcher . 
  M.mapPos (\(x, y) tiles -> RS.shuffle' tiles (length tiles) (genFn x y)) .
  M.extendTo tileSet ((M.nrows idMap) + width) ((M.ncols idMap) + height) .
  fmap L.singleton . M.transpose $ idMap
    where imageMap = splitImageMap tileSize origImage
          lm       = makeLookupMap idMap
          idll     = makeIdLookupList . M.toList $ imageMap 
          idMap    = fmap (\e -> maybe (-1) id $ lookup e idll) imageMap
          matcher  = makeTileMatcher lm
          tileSet  = map (snd) idll
          genFn x y = R.mkStdGen (seed + (x + y * (M.ncols idMap + width * tileSize)))

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

makeIdLookupList :: (Eq a) => [a] -> [(a, Int)]
makeIdLookupList = 
  (flip zip) [0..] .
  L.nub
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

allTiles :: (Eq a) => [((a,a), b)] -> [a]
allTiles input = L.nub (input >>= ((\a -> [fst a, snd a]) . fst))

match :: TileMatcher -> Side -> ImageId -> ImageId -> Bool
match mtrx side im1 im2 = elem side (mtrx M.! ((im1 + 1), (im2 + 1)))

makeTileMatcher :: [((ImageId, ImageId), [Side])] -> TileMatcher
makeTileMatcher lookupList = mtrx
  where size = length . allTiles $ lookupList
        mtrx = M.matrix size size (\(x,y) -> maybe [] id $ lookup ((x - 1), (y - 1)) lookupList) 
