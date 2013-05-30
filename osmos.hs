import Data.List

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
osmosSolve armin motes = length $ take (length motes) (step armin (sort motes))

step :: Int -> [Int] -> [Int]
step _ [] = []
step armin (x:xs)
    | armin > x = let (newArmin, remaining) = consume armin (x:xs) in step newArmin remaining
    | armin <= x = let nextSteps = bridge armin x in nextSteps ++ step (last nextSteps) (x:xs)

bridge :: Int -> Int -> [Int]
bridge armin nextMote
    | armin > nextMote = []
    | armin <= nextMote = let nextArmin = 2 * armin - 1 in nextArmin : bridge (nextArmin) nextMote 

consume :: Int -> [Int] -> (Int, [Int])
consume armin motes = (partialsums !! consumedLen, drop consumedLen motes)
    where partialsums = (scanl (\c x -> c + x) armin motes)
          incdiffs = map (\(a, b) -> a - b) (zip partialsums motes)
          consumedLen = length $ takeWhile ((<) 0) incdiffs
