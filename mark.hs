-- Q3. Student Academic Performance Report
-- Alice:35
-- Bob:55

import System.IO

-- Classification function with guards
classify :: Int -> String
classify mark
  | mark < 40  = "Fail"
  | mark < 60  = "Pass"
  | mark < 80  = "Merit"
  | otherwise  = "Distinction"

-- Recursive function to categorize students
categorize :: [(String, Int)] -> [(String, Int, String)]
categorize [] = []
categorize ((name, mark):xs) = (name, mark, classify mark) : categorize xs

-- Recursive function to count passes
countPasses :: [(String, Int)] -> Int
countPasses [] = 0
countPasses ((_, mark):xs)
  | mark >= 40 = 1 + countPasses xs
  | otherwise  = countPasses xs

-- Built-in sample data
sampleData :: [(String, Int)]
sampleData =
  [ ("Alice", 35)
  , ("Bob", 55)
  , ("Charlie", 72)
  , ("Diana", 85)
  , ("Eve", 40)
  ]

-- Parse input like "Alice:55"
parseInput :: [String] -> [(String, Int)]
parseInput [] = []
parseInput (line:rest) =
  case wordsWhen (==':') line of
    [name, markStr] -> (name, read markStr :: Int) : parseInput rest
    _               -> parseInput rest

-- Helper to split by delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

-- Print report
printReport :: [(String, Int)] -> IO ()
printReport students = do
  let categorized = categorize students
  let passCount = countPasses students
  putStrLn "\n--------------------------------------"
  putStrLn " Student Academic Performance Report"
  putStrLn "--------------------------------------"
  mapM_ (\(n,m,c) -> putStrLn (n ++ " (" ++ show m ++ "): " ++ c)) categorized
  putStrLn "--------------------------------------"
  putStrLn ("Total Students: " ++ show (length students))
  putStrLn ("Students Passed (>=40): " ++ show passCount)
  putStrLn "--------------------------------------"
  putStrLn "End of Report"

-- Main logic
main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  if length inputLines == 1 && head inputLines == "NIL"
    then printReport sampleData
    else printReport (parseInput inputLines)
