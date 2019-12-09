{-#LANGUAGE LambdaCase#-}
module LukeOtte (module LukeOtte) where

import Prelude hiding (Either(..))

data Either a b c = Left a | Mid b | Right c

data Maskin = Maskin {
    hjulNull :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulEn :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulTo :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulTre :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulFire :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulFem :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulSeks :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulSyv :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulÅtte :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)],
    hjulNi :: [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)]
}

maskin :: Maskin
maskin = Maskin {
    hjulNull = cycle [Left pluss101, Left opp7, Left minus9, Left pluss101],
    hjulEn = cycle [Left trekk1FraOdde, Left minus1, Left minus9, Left pluss1TilPar],
    hjulTo = cycle [Left pluss1TilPar, Left pluss4, Left pluss101, Left minus9],
    hjulTre = cycle [Left minus9, Left pluss101, Left trekk1FraOdde, Left minus1],
    hjulFire = cycle [Right roterOdde, Left minus1, Left pluss4, Right roterAlle],
    hjulFem = cycle [Left gangeMSD, Left pluss4, Left minus9, Mid stopp],
    hjulSeks = cycle [Left minus1, Left pluss4, Left minus9, Left pluss101],
    hjulSyv = cycle [Left pluss1TilPar, Left minus9, Left trekk1FraOdde, Left deleMSD],
    hjulÅtte = cycle [Left pluss101, Left reverserSiffer, Left minus1, Right roterPar],
    hjulNi = cycle [Left pluss4, Left gangeMSD, Left reverserSiffer, Left minus9]
}

hentHjul :: Maskin -> Integer -> [Either (Integer -> Integer) (Integer -> Integer) (Maskin -> Maskin)]
hentHjul m int = \case
    0 -> hjulNull m
    1 -> hjulEn m
    2 -> hjulTo m
    3 -> hjulTre m
    4 -> hjulFire m
    5 -> hjulFem m
    6 -> hjulSeks m
    7 -> hjulSyv m
    8 -> hjulÅtte m
    9 -> hjulNi m
    $ mod int 10

roterHjul :: Maskin -> Integer -> Maskin
roterHjul m int = \case
    0 -> roterHjulNull m
    1 -> roterHjulEn m
    2 -> roterHjulTo m
    3 -> roterHjulTre m
    4 -> roterHjulFire m
    5 -> roterHjulFem m
    6 -> roterHjulSeks m
    7 -> roterHjulSyv m
    8 -> roterHjulÅtte m
    9 -> roterHjulNi m
    $ mod int 10

roterHjulNull, roterHjulEn, roterHjulTo, roterHjulTre, roterHjulFire, roterHjulFem, roterHjulSeks, roterHjulSyv, roterHjulÅtte, roterHjulNi :: Maskin -> Maskin
roterHjulNull m = Maskin { hjulNull = tail $ hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulEn m = Maskin { hjulNull = hjulNull m, hjulEn = tail $ hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulTo m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = tail $ hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulTre m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = tail $ hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulFire m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = tail $ hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulFem m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = tail $ hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulSeks m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = tail $ hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulSyv m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = tail $ hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = hjulNi m}
roterHjulÅtte m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = tail $ hjulÅtte m, hjulNi = hjulNi m}
roterHjulNi m = Maskin { hjulNull = hjulNull m, hjulEn = hjulEn m, hjulTo = hjulTo m, hjulTre = hjulTre m, hjulFire = hjulFire m, hjulFem = hjulFem m, hjulSeks = hjulSeks m, hjulSyv = hjulSyv m, hjulÅtte = hjulÅtte m, hjulNi = tail $ hjulNi m}

pluss4, pluss101, minus9, minus1 :: Integer -> Integer
pluss4 = (+4)
pluss101 = (+101)
minus9 = (+(-9))
minus1 = (+(-1))

reverserSiffer :: Integer -> Integer
reverserSiffer int = (*(signum int)) .  read . reverse . show . abs $ int

opp7 :: Integer -> Integer
opp7 int
    | mod int 10 == 7 = int
    | otherwise = opp7 $ int + 1

gangeMSD, deleMSD :: Integer -> Integer
gangeMSD int = (*) int $ read . pure . head . show . abs $ int
deleMSD int = div int $ read . pure . head . show . abs $ int  

pluss1TilPar, trekk1FraOdde :: Integer -> Integer
pluss1TilPar int = (*(signum int)) . read . concat $ show . (\x -> \case
    0 -> x+1
    1 -> x 
    $ mod x 2) . read . pure <$> (show . abs $ int)
trekk1FraOdde int = (*(signum int)) . read . concat $ show . (\x -> \case
    0 -> x
    1 -> x-1
    $ mod x 2) . read . pure <$> (show . abs $ int)

roterPar, roterOdde, roterAlle :: Maskin -> Maskin
roterPar = roterHjulÅtte . roterHjulSeks . roterHjulFire . roterHjulTo . roterHjulNull
roterOdde = roterHjulNi . roterHjulSyv . roterHjulFem . roterHjulTre . roterHjulEn
roterAlle = roterPar . roterOdde

stopp :: Integer -> Integer
stopp = id

spill :: Maskin -> Integer -> Integer
spill m int = let nyMaskin = roterHjul m int
                  hjul = hentHjul m int
              in \case
                    Mid f -> f int
                    Left f -> spill nyMaskin $ f int
                    Right f -> spill (f nyMaskin) int
                    $ head hjul

printLøsningÅtte :: IO ()
printLøsningÅtte = putStrLn . show $ zipWith (\a b -> show b <> " mynter: " <> show a) (spill maskin <$> [1..9]) [1..9]