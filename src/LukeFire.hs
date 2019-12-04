{-#LANGUAGE OverloadedStrings, FlexibleContexts#-}
module LukeFire (printLøsningFire) where

import qualified Data.Text as T
import System.Directory

--infixr 2 £
--(£) = ($)

infixr 6 £
(£) = ($)

getFile :: IO FilePath
getFile = getCurrentDirectory
    >>= pure . (drop 1) . T.unpack . (foldl (\str acc -> str <> "\\" <> acc ) "") . (<> [filename]) . (takeWhile (/= "Kodekalender")) . (T.splitOn "\\") . T.pack
    where filename = "Kodekalender\\Filer\\coords.csv"

tupUnpack :: (T.Text,T.Text) -> (String, String)
tupUnpack (text,txet)= (T.unpack $ text,  T.unpack $ txet)

tupRead :: Read a => (String, String) -> (a, a)
tupRead (text, txet) = (read text, read txet)

parseFile :: FilePath -> IO [(Int, Int)]
parseFile path = readFile path >>= \str -> pure $ tupRead . (fmap tail) . tupUnpack . T.breakOn "," <$> init . tail . (T.splitOn "\n") . T.pack £ str

initMatrix :: [[Int]]
initMatrix = replicate 1000 $ take 1000 [1,1..]

changeAt :: Int -> (a -> a) -> [a] -> [a]
changeAt _ _ [] = []
changeAt 0 f (l:ls)= (f l):ls
changeAt i f (l:ls)= l:(changeAt (i-1) f ls)

changeAt' :: (Int,Int) -> (a -> a) -> [[a]] -> [[a]]
changeAt' _ _ [] = []
changeAt' (i,0) f (l:ls) = (changeAt i f l):ls
changeAt' (i,j) f (l:ls) = l:(changeAt' (i,j-1) f ls)

(?!) :: [[a]] -> (Int, Int) -> a
mtx ?! (i,j) = mtx !! j !! i

type Snail = (Int, Int)
type Time = Int
type Map = [[Int]]

moveUp, moveDown, moveLeft, moveRight :: (Snail,Map,Time) -> (Snail,Map,Time)
moveUp ((i,j),ls,t) = ((i-1,j),changeAt' (i,j) (+1) ls,t + ls ?! (i-1,j))
moveDown ((i,j),ls,t) = ((i+1,j),changeAt' (i,j) (+1) ls,t + ls ?! (i+1,j))
moveLeft ((i,j),ls,t) = ((i,j-1),changeAt' (i,j) (+1) ls,t + ls ?! (i,j-1))
moveRight ((i,j),ls,t) = ((i,j+1),changeAt' (i,j) (+1) ls,t + ls ?! (i,j+1))

start :: (Snail,Map,Time)
start = ((0,0),initMatrix,0)

walk :: (Snail,Map,Time) -> (Int,Int) -> (Snail,Map,Time)
walk (s,m,t) coord
    | fst s > fst coord = flip walk coord $ moveUp (s,m,t)
    | fst s < fst coord = flip walk coord $ moveDown (s,m,t)
    | snd s < snd coord = flip walk coord $ moveRight (s,m,t)
    | snd s > snd coord = flip walk coord $ moveLeft (s,m,t)
    | otherwise = (s,m,t)

thd :: (a,b,c) -> c
thd (a,b,c) = c

printLøsningFire :: IO ()
printLøsningFire = getFile >>= parseFile >>= putStrLn . show . thd . (foldl walk start)