{-#LANGUAGE OverloadedStrings, BangPatterns#-}
module LukeSeks (printLøsningSeks) where

import Data.Bits
import qualified Graphics.Image as I

import qualified Data.Text as T
import System.Directory

import Control.Monad
import Control.Applicative
import Data.List.Split (chunksOf)

getFile :: IO FilePath
getFile = 
    getCurrentDirectory
    >>= pure 
        . (drop 1) 
        . T.unpack 
        . (foldl (\str acc -> str <> "\\" <> acc ) "") 
        . (<> [filename]) 
        . (takeWhile (/= "Kodekalender")) 
        . (T.splitOn "\\") 
        . T.pack
    where filename = "Kodekalender\\Filer\\mush.png"

newFile :: IO FilePath
newFile = 
    getCurrentDirectory
    >>= pure 
        . (drop 1) 
        . T.unpack 
        . (foldl (\str acc -> str <> "\\" <> acc ) "") 
        . (<> [filename]) . (takeWhile (/= "Kodekalender")) 
        . (T.splitOn "\\") 
        . T.pack
    where filename = "Kodekalender\\Filer\\unMush.png"

parseFile :: FilePath -> IO [[I.Pixel I.RGB Double]]
parseFile path = I.readImageRGB I.VU path >>= pure . I.toLists

toInt :: [[I.Pixel I.RGB Double]] -> [[I.Pixel I.RGB Int]]
toInt = fmap . fmap . fmap $ round . (*255)

toDouble :: [[I.Pixel I.RGB Int]] -> [[I.Pixel I.RGB Double]]
toDouble = fmap . fmap . fmap $ (/255) . fromIntegral

unMush :: [[I.Pixel I.RGB Int]] -> [[I.Pixel I.RGB Int]]
unMush lss = chunksOf chunk $ zipWith (liftA2 xor) ls $ (I.PixelRGB 0 0 0):ls
    where ls = join lss
          chunk = head $ length <$> take 1 lss

printLøsningSeks :: IO ()
printLøsningSeks = 
    getFile 
    >>= parseFile 
    >>= \lss -> newFile
    >>= \path -> (I.writeImage path)  
        . (I.fromLists :: [[I.Pixel I.RGB Double]] -> I.Image I.VS I.RGB Double) 
        . toDouble 
        . unMush 
        . toInt
        $ lss