{-#LANGUAGE DeriveFunctor, ViewPatterns#-}
module LukeFjorten where

import Data.Foldable
import Control.Monad

infixl 8 :<
data Zippy a = a:<([a],[a]) | Empty deriving(Show, Read, Eq, Functor)

instance Foldable Zippy where
    foldr f acc zipz = foldr f acc $ unzippy zipz

instance Semigroup (Zippy a) where
    (a:<(as,bs))<>(unzippy -> cs) = a:<(as,bs<>cs) 

append :: Zippy a -> [a] -> Zippy a
append (a:<(as,bs)) cs = a:<(as,bs<>cs)

zippy :: [a] -> Zippy a
zippy [] = Empty
zippy (l:ls) = l:<([],ls)

unzippy :: Zippy a -> [a]
unzippy Empty = []
unzippy (l:<(ls,sl)) = reverse (l:ls) <> sl

view :: Zippy a -> a
view (l:<_) = l

trace, retrace :: Zippy a -> Zippy a
trace (v:<(as,b:bs)) = b:<(v:as,bs)
retrace (v:<(a:as,bs)) = a:<(as,v:bs)

lengthZ :: Zippy a -> Int
lengthZ (a:<(as,bs)) = 1 + length as + length bs

filter' :: (a -> Bool) -> Zippy a -> Zippy a
filter' _ Empty = Empty
filter' f (a:<(as,bs)) = (head ls):<(tail ls,filter f bs)
    where ls = filter f $ a:as

initialise :: [Int] -> (Int, [Int], [Int])
initialise lss@(l:ls) = (length $ ps, ps, tail . cycle $ lss)
    where ps = replicate l l

makeSeq :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
makeSeq _ _ _ cs [] = cs
makeSeq end len i cs (l:ls)
    | len < end = makeSeq end (len + h) (i+1) (cs<>next) ls
    | otherwise = cs
    where next = replicate h l
          h = cs !! i

loop :: Int -> [Int] -> [Int]
loop end (initialise -> (len, cs, ls)) = makeSeq end len 1 cs ls 

start :: [Int]
start = [2,3,5,7,11]

end :: Int
end = 217532235

printLøsningFjorten :: IO ()
printLøsningFjorten = putStrLn . show $ foldr' (+) 0 $ filter (==7) $ loop end start

test :: Int -> [Int]
test x = loop x [2,3,5,7]  

nDir :: [Int] -> [Int]
nDir alfa = join $ map (\i -> case i of 
        0 -> replicate (alfa !! 0) (alfa !! 0)
        x -> replicate (nDir alfa !! x) (alfa !! (mod x len))) [0..]
        where len = length alfa