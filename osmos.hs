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
osmosSolve armin motes = head $ sort (n: if armin >= 2 then solutions else [])
    where n = length motes
          solutions = map (\(x, y) -> x + y) $ zip (scanl1 (+) costs) remainders
          costs = map fst steps
          remainders = map snd steps
          steps = step armin (sort motes)

step :: Int -> [Int] -> [(Int, Int)]
step _ [] = []
step armin (x:xs)
    | armin > x = let (newArmin, remaining) = consume armin (x:xs)
                  in (0, length remaining) : step newArmin remaining
    | armin <= x = let bridgeSteps = bridge armin x; bridgedArmin = last bridgeSteps; cost = length bridgeSteps
                       (newArmin, remaining) = consume bridgedArmin (x:xs)
                   in (cost, length remaining) : step newArmin remaining

bridge :: Int -> Int -> [Int]
bridge armin nextMote
    | armin > nextMote = []
    | armin <= nextMote = let nextArmin = 2 * armin - 1 in nextArmin : bridge nextArmin nextMote 

consume :: Int -> [Int] -> (Int, [Int])
consume armin motes = (partialsums !! consumedLen, drop consumedLen motes)
    where partialsums = (scanl (+) armin motes)
          incdiffs = map (\(a, b) -> a - b) (zip partialsums motes)
          consumedLen = length $ takeWhile ((<) 0) incdiffs
