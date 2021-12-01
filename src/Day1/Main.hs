module Day1.Main (parse, solveA, solveB) where
    
parse :: String -> [Int]
parse = fmap read . lines

solveA :: [Int] -> Int
solveA ns = length $ filter (< 0) $ zipWith (-) ns $ tail ns

solveB :: [Int] -> Int
solveB = undefined