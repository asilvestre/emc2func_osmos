import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map


main = do
	numEntries <- getLine
	processLine 1

processLine i = do
    info <- getLine
    dat <- getLine
    putStrLn $ "Case #" ++ (show i) ++ ": " ++ (show $ osmosMain info dat)
    processLine (i + 1)

osmosMain :: String -> String -> Int
osmosMain info dat = osmosSolve armin motes
    where armin = read (head $ words info)
          motes = map read (words dat)

osmosSolve :: Int -> [Int] -> Int
osmosSolve armin motes = osmosStep (length motes) armin motes

osmosStep :: Int -> Int -> [Int] -> Int
osmosStep max _ [] = 0
osmosStep max armin (x:xs)
    | armin > x = let (newArmin, remaining) = consume armin (x:xs) in osmosStep max newArmin remaining
    | armin <= x = let newSteps = osmosBridge armin x in if length (take max newSteps) < max then length newSteps + osmosStep max (last newSteps) (x:xs) else max

osmosBridge :: Int -> Int -> [Int]
osmosBridge armin nextMote
    | armin > nextMote = []
    | armin <= nextMote = let nextArmin = 2 * armin - 1 in nextArmin : osmosBridge (nextArmin) nextMote 

--Returns the motes not consumed and armin's size
consume :: Int -> [Int] -> (Int, [Int])
consume armin motes = (partialsums !! consumedLen, drop consumedLen motes)
    where partialsums = (scanl (\c x -> c + x) armin motes)
          incdiffs = map (\(a, b) -> a - b) (zip partialsums motes)
          consumedLen = length $ takeWhile ((<) 0) incdiffs
