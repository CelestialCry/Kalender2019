{-#LANGUAGE OverloadedStrings#-}
module LukeTretten (printLøsningTretten) where

import qualified Data.Text as T
import qualified Data.Matrix as M
import Data.Matrix ((!))
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import System.Directory
import Control.Monad
import Prelude hiding (Either(..))

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
    where filename = "Kodekalender\\Filer\\lab.txt"

parseFile :: FilePath -> IO (M.Matrix Rute)
parseFile path = readFile path >>= pure . M.fromLists . read . join . (map swish)

swish :: Char -> String
swish ':' = "="
swish '"' = ""
swish '{' = "R{"
swish x = [x]

data Rute = R {
    x :: Int,
    y :: Int,
    top :: Bool,
    left :: Bool,
    bottom :: Bool,
    right :: Bool
} deriving(Show, Read)

data Retning = Top | Left | Bottom | Right deriving(Show, Read, Eq)

getWall :: Rute -> Retning -> Bool
getWall kart Top = not $ top kart
getWall kart Left = not $ left kart
getWall kart Bottom = not $ bottom kart
getWall kart Right = not $ right kart

data One = O deriving(Show, Read, Eq)

data Robot = B {
    x' :: Int,
    y' :: Int,
    visited :: Map.Map (Int,Int) One,
    trail :: [(Int,Int)],
    visitationRule :: [Retning],
    n :: Int
} deriving(Show, Read, Eq)

mToBool :: Maybe a -> Bool
mToBool Nothing = True
mToBool _ = False

move :: Retning -> Robot -> Robot
move Top rob = B {x' = x' rob - 1, y' = y' rob, visited = Map.insert (x' rob, y' rob) O $ visited rob, trail = (x' rob, y' rob):trail rob, visitationRule = visitationRule rob, n = n rob + 1}
move Left rob = B {x' = x' rob, y' = y' rob - 1, visited = Map.insert (x' rob, y' rob) O $ visited rob, trail = (x' rob, y' rob):trail rob, visitationRule = visitationRule rob, n = n rob + 1}
move Bottom rob = B {x' = x' rob + 1, y' = y' rob, visited = Map.insert (x' rob, y' rob) O $ visited rob, trail = (x' rob, y' rob):trail rob, visitationRule = visitationRule rob, n = n rob + 1}
move Right rob = B {x' = x' rob, y' = y' rob + 1, visited = Map.insert (x' rob, y' rob) O $ visited rob, trail = (x' rob, y' rob):trail rob, visitationRule = visitationRule rob, n = n rob + 1} 

walk :: Rute -> Robot -> [Retning] -> Robot
walk rute rob (a:b:c:d:[])
    | (getWall rute a) && (mToBool $ (visited rob) !? (moveTo a rob)) = move a rob
    | (getWall rute b) && (mToBool $ (visited rob) !? (moveTo b rob)) = move b rob
    | (getWall rute c) && (mToBool $ (visited rob) !? (moveTo c rob)) = move c rob
    | (getWall rute d) && (mToBool $ (visited rob) !? (moveTo d rob)) = move d rob
    | otherwise = B {x' = fst l, y' = snd l, visited = Map.insert (x' rob, y' rob) O $ visited rob, trail = tail $ trail rob, visitationRule = visitationRule rob, n = n rob}
    where moveTo ret rob = (x' $ move ret rob, y' $ move ret rob)
          l = head $ trail rob

walkLikeAnEgyptian :: M.Matrix Rute -> (Int,Int) -> Robot -> Int
walkLikeAnEgyptian kart end rob
    | end == (x' rob, y' rob) = n rob
    | otherwise = walkLikeAnEgyptian kart end $ walk (kart!(x' rob, y' rob)) rob $ visitationRule rob

testerMap :: M.Matrix Rute
testerMap = M.fromLists $
    [[R{x=0,y=0,top=True,left=True,bottom=False,right=False}, R{x=0,y=1,top=True,left=False,bottom=False,right=True},R{x=0,y=2,top=True,left=True,bottom=False,right=True}],
    [R{x=1,y=0,top=False,left=True,bottom=False,right=False}, R{x=1,y=1,top=False,left=False,bottom=True,right=False},R{x=1,y=2,top=False,left=False,bottom=True,right=True}],
    [R{x=2,y=0,top=False,left=True,bottom=True,right=False}, R{x=2,y=1,top=True,left=False,bottom=True,right=False}, R{x=2,y=2,top=True,left=False,bottom=True,right=True}]]

startArt :: Robot
startArt = B {
    x' = 1,
    y' = 1,
    visited = Map.fromAscList [],
    trail = [],
    visitationRule = [Bottom, Right, Left, Top],
    n = 0
}

startIsa :: Robot
startIsa = B {
    x' = 1,
    y' = 1,
    visited = Map.fromAscList [],
    trail = [],
    visitationRule = [Right, Bottom, Left, Top],
    n = 0
}

testWalk :: Int
testWalk = walkLikeAnEgyptian testerMap (3,3) startIsa

printLøsningTretten :: IO ()
printLøsningTretten = getFile >>= parseFile >>= \kart -> putStrLn . show . abs $ walkLikeAnEgyptian kart end startArt - walkLikeAnEgyptian kart end startIsa
    where end = (500,500)