module Main (main) where

import qualified System.Directory      as SD
import qualified System.IO             as SIO
import qualified System.Process        as P
import qualified System.Random         as R

import qualified Data.Matrix           as M
import qualified Data.Maybe            as Mb
import qualified Data.List             as L

import qualified Control.Monad         as CM

import           Data.Time.Clock.POSIX

import           Tiles


main :: IO ()
main = do
    let tileDirectory = "./tiles"
    files <- SD.getDirectoryContents tileDirectory
    
    images <- sequenceA $ map (\fileName -> loadImage (tileDirectory ++ "/" ++ fileName) ) $ filter (\n -> (reverse . (take 3) . reverse $ n) == "ppm") files 
    
    let fullImages = L.concat . (map ((take 4) . iterate rotate)) $ images
    
    t <- ((round . (* 1000000)) <$> getPOSIXTime)

    (SIO.writeFile "generatedImage.ppm") . showMatrix . mergeTileMap $ generateTileMap t fullImages 20 20
    CM.void $ P.runCommand "gimp generatedImage.ppm"

