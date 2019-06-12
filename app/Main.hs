module Main where

import Lib

import System.Environment
import Data.List
import Data.Tuple
import Data.Char
import System.Exit
import System.IO
import Control.Monad
import Debug.Trace
import Text.Printf

main :: IO ()
main = do
        args <- getArgs
        case args of
             [color_number, convergence, filepath]  -> imageCompressor (read color_number:: Int) (read convergence:: Double) filepath
             otherwise -> putStrLn("usage")

calculateColorAverage :: [Pixel] -> Color -> Double -> Color
calculateColorAverage [] color_total index = divideColor color_total index
calculateColorAverage (pix:pixels) color_total index = calculateColorAverage pixels color_total index
                    where
                        index = index + 1
                        color_total = addColor color_total (getColor pix)

divideColor :: Color -> Double -> Color
divideColor (Color rr gg bb) diviser = Color (r) (g) (b)
                            where
                                r = rr / diviser
                                g = gg / diviser
                                b = bb / diviser

imageCompressor :: Int -> Double -> String -> IO()
imageCompressor color_number convergence filepath = do
                    s     <- readFile filepath
                    let tab = linesplitter(s)
                    let pixels = parsetab tab []
                    let clusters = createCluster color_number pixels []
                    let result = mainLoop clusters convergence pixels
                    printtab result

-- C'est quoi Val ? putain !!!!!!!!!
verificationConvergence :: [Color] -> [Color] -> Double -> Int -> Bool
verificationConvergence [] [] conv val
    | val == 0 = False
    | otherwise = True
verificationConvergence (bcluster:bclusters) (ncluster:nclusters) conv val
    | distColor bcluster ncluster > conv = verificationConvergence bclusters nclusters conv (val + 1)
    | otherwise = verificationConvergence [] [] conv 0

addColor :: Color -> Color -> Color
addColor (Color r1 g1 b1) (Color r2 g2 b2) = (Color (r1 + r2) (g1 + g2) (b1 + b2))


calculateMoyenne :: [Pixel] -> Double -> Color -> Color
calculateMoyenne [] val rep = divideColor rep val
calculateMoyenne (pix:pixel) val rep = calculateMoyenne pixel (val + 1) (addColor rep (getColor pix))

calculateNewCluster :: [Color] -> [[Pixel]] -> [Color]
calculateNewCluster rep [] = rep
calculateNewCluster rep (arpix:pixels) = calculateNewCluster (rep ++ [calculateMoyenne arpix 0 (Color 0 0 0)]) pixels

mainLoop :: [Color] -> Double -> [Pixel] -> [[Pixel]]
mainLoop clusters convergence pixels = do
    let group = groupByCluster clusters pixels $ createArrayClusters clusters []
    let newcluster = calculateNewCluster [] group
    verconvergence <- [verificationConvergence clusters newcluster convergence 0]
    case verconvergence of
        True -> mainLoop newcluster convergence pixels
        otherwise -> group

createSimpleArray :: Color -> [Pixel]
createSimpleArray color = [(Pixel (Position (-1) (-1)) color)]

createArrayClusters :: [Color] -> [[Pixel]] -> [[Pixel]]
createArrayClusters [] rep = rep
createArrayClusters (col:colors) rep = createArrayClusters colors (rep ++ [(createSimpleArray col)])

groupByCluster :: [Color] -> [Pixel] -> [[Pixel]] -> [[Pixel]]
groupByCluster _ [] rep = rep
groupByCluster clusters (pix:pixel) rep = groupByCluster clusters pixel $ addPixelArrayCluster clusters clusters pix rep 1 1

addPixelArrayCluster :: [Color] -> [Color] -> Pixel -> [[Pixel]] -> Int -> Int -> [[Pixel]]
addPixelArrayCluster _ [] pix rep _ pos = (fst (splitAt (pos - 1) rep)) ++ [(pix:(head (snd (splitAt (pos - 1) rep))))] ++ (tail (snd (splitAt (pos - 1) rep)))
addPixelArrayCluster allcluster (cluster:clusters) pixel rep ver nbcluster
    | nbcluster == 0 = addPixelArrayCluster allcluster clusters pixel rep (ver + 1) (nbcluster + 1)
    | otherwise = addPixelArrayCluster allcluster clusters pixel rep (ver + 1) $ checkChangeCluster nbcluster ver (distanceClusterPixel allcluster pixel 1 nbcluster) (distColor cluster (getColor pixel))

checkChangeCluster :: Int -> Int -> Double -> Double -> Int
checkChangeCluster val index currentcluster newcluster
    | currentcluster < newcluster = val
    | otherwise = index

distanceClusterPixel :: [Color] -> Pixel -> Int -> Int -> Double
distanceClusterPixel [] _ _ _ = 255
distanceClusterPixel (cluster:clusters) pixel poscluster nbcluster
    | poscluster == nbcluster = distColor cluster $ getColor pixel
    | otherwise = distanceClusterPixel clusters pixel (poscluster + 1) nbcluster


createCluster :: Int -> [Pixel] -> [Color] -> [Color]
createCluster 0 _ clusters = clusters
createCluster _ [] clusters = clusters
createCluster number (pix:pixels) clusters
    | elem (getColor pix) clusters == False = createCluster (number - 1) pixels ((getColor pix):clusters)
    | otherwise = createCluster number pixels clusters

parsetab :: [String] -> [Pixel] -> [Pixel]
parsetab [] pixels = pixels
parsetab (x:xs) pixels = parsetab xs antilope
                where
                    [(pix, str)] = (readsPrec 0 x):: [(Pixel, String)]
                    antilope = pix:pixels

-- printtab :: Show a => [a] -> IO()
-- printtab [] = return ()
-- printtab (x:xs) = do
--     putStrLn (filter (/='\"')(show x))
--     printtab xs

theEnd :: [Pixel] -> IO()
theEnd [] = return ()
theEnd (pixel:pixels)
    | getPos pixel == (Position (-1) (-1)) = do
        putStrLn (filter (/='\"')("--"))
        putStrLn (filter (/='\"')(extendColor (getColor pixel)))
        putStrLn (filter (/='\"')("-"))
        theEnd pixels
    | otherwise = do
        putStrLn (filter (/='\"')(show pixel))
        theEnd pixels

printtab :: [[Pixel]] -> IO()
printtab [] = return ()
printtab (pixel:pixels) = do
    theEnd $ reverse pixel
    printtab pixels

linesplitter :: String -> [String]
linesplitter line = wordsWhen (=='\n') line

getColor :: Pixel -> Color
getColor (Pixel _ c1) = c1

getPos :: Pixel -> Position
getPos (Pixel p1 _) = p1

data Position = Position Int Int

instance Show Position where
    show (Position x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Eq Position where
    (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

instance Read Position where
    readsPrec _ input = [(Position (read (takeX input)) (read (takeY input)), takeRest input)]
        where
            takeX :: String -> String
            takeX = takeWhile (/= ',') . tail . dropWhile (/='(')
            takeY :: String -> String
            takeY = takeWhile (/= ')') . tail . dropWhile (/=',')
            takeRest :: String -> String
            takeRest = tail . dropWhile (/= ')')

extendColor :: Color -> [Char]
extendColor (Color r g b) = "(" ++ specialshow r ++ "," ++ specialshow g ++ "," ++ specialshow b ++ ")"

specialshow :: Double -> String
specialshow nb = printf "%.2f" (nb :: Double)

data Color = Color Double Double Double
instance Show Color where
    show (Color r g b) = "(" ++ (takeWhile (/= '.') (show r)) ++ "," ++ (takeWhile (/= '.') (show g)) ++ "," ++ (takeWhile (/= '.') (show b)) ++ ")"

instance Eq Color where
    (Color x1 y1 z1) == (Color x2 y2 z2 ) = x1 == x2 && y1 == y2 && z1 == z2

instance Read Color where
    readsPrec _ input = [(Color (read (takeR input)) (read (takeG input)) (read (takeB input)), takeRest input)]
        where
            takeR :: String -> String
            takeR = takeWhile (/= ',') .tail .dropWhile (/='(')
            takeG :: String -> String
            takeG = takeWhile (/= ',') .tail .dropWhile (/=',')
            takeB :: String -> String
            takeB = takeWhile (/=')') . tail . dropWhile (/= ',') .tail .dropWhile (/=',')
            takeRest :: String -> String
            takeRest = tail . dropWhile (/= ')')



data Pixel = Pixel Position Color
instance Show Pixel where
    show (Pixel pos color) = show pos ++ " " ++ show color

instance Read Pixel where
    readsPrec _ input = [(Pixel pos color, rest2)]
        where
            [(pos, rest1)] = readsPrec 0 input :: [(Position, String)]
            [(color, rest2)] = readsPrec 0 rest1 :: [(Color, String)]

distPixel :: Pixel -> Pixel -> Double
distPixel (Pixel _ c1) (Pixel _ c2) = distColor c1 c2

distColor :: Color -> Color -> Double
distColor (Color r1 g1 b1) (Color r2 g2 b2) = sqrt(((r1 - r2) ^ 2) + ((g1 - g2) ^ 2) + ((b1 - b2) ^ 2))



-- Boostrap

distance (r1, g1, b1) (r2, g2, b2) = sqrt(((r1 - r2) ^ 2) + ((g1 - g2) ^ 2) + ((b1 - b2) ^ 2))

removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` "( )\"\'") ]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

mySplit :: String -> [String]
mySplit line = wordsWhen (==',') line

castToFloat :: String -> Float
castToFloat s = read s :: Float

closestFind :: [[String]] -> (Float, Float, Float) -> Float -> (Int, Int, Int)
closestFind [] (n1, n2, n3) mini = (1, 1, 1)
closestFind (x:xs) (n1, n2, n3) mini
    | (distance (read(x!!0), read(x!!1), read(x!!2)) (n1, n2, n3)) == mini = (read(x!!0), read(x!!1), read(x!!2))
    | otherwise = closestFind xs (n1, n2, n3) mini

closestList :: [[String]] -> (Float, Float, Float) -> Float -> Float
closestList [] (n1, n2, n3) mini = mini
closestList (x:xs) (n1, n2, n3) mini
    | (distance (read(x!!0), read(x!!1), read(x!!2)) (n1, n2, n3)) < mini = closestList xs (n1, n2, n3) (distance (read(x!!0), read(x!!1), read(x!!2)) (n1, n2, n3))
    | otherwise = closestList xs (n1, n2, n3) mini

remove element list = filter (\e -> e/=element) list

closest :: String -> (Float, Float, Float) -> IO ()
closest filename (r1, g1, b1) = do
    contents <- readFile filename
    let list =  map mySplit (map removePunc (lines contents))
    let num = (closestList list (r1, g1, b1) 100000)
    print (closestFind list (r1, g1, b1) num)

outlierCalc :: [[String]] -> [String] -> Int -> Int -> Float -> Float
outlierCalc [] c i pos res = res
outlierCalc (x:xs) c i pos res
    | i /= pos = outlierCalc xs c (i + 1) pos (res + (distance (read(x!!0), read(x!!1), read(x!!2)) (read(c!!0), read(c!!1), read(c!!2))))
    | otherwise = outlierCalc xs c (i + 1) pos res

outlierLoop :: [[String]] -> [[String]] -> Int -> [Float] -> [Float]
outlierLoop [] list nb res = res
outlierLoop (x:xs) list nb res = outlierLoop xs list (nb + 1) (res ++ [outlierCalc list (list!!nb) 0 nb 0])



expelOutlier :: String -> IO ()
expelOutlier filename = do
    contents <- readFile filename
    let list =  map mySplit (map removePunc (lines contents))
    let list2 = (outlierLoop list list 0 [])
    print (elemIndex (maximum list2) list2)

principalFunc :: IO ()
principalFunc = do
        args <- getArgs
        print (distance (3,4,5) (3,8,2))

f :: [String] -> [Int]
f = map read
