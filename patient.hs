-- Q1. Hospital Patient Records Analysis
-- -------------------------------------

-- Raj:45:2
-- Meera:19:1
-- Sam:12:3

import Data.Char (isSpace, toUpper)

type Patient = (String, Int, Int)

-- Built-in sample data
sampleData :: [Patient]
sampleData =
    [ ("Alice", 15, 1)
    , ("Bob", 40, 2)
    , ("Charlie", 17, 1)
    , ("Diana", 60, 3)
    , ("Eva", 33, 2)
    , ("Frank", 18, 3)
    , ("Grace", 45, 1)
    ]

-- Recursive count by ReasonCode (pattern matching)
countReason :: Int -> [Patient] -> Int
countReason _ [] = 0
countReason code ((_,_,r):xs)
    | r == code = 1 + countReason code xs
    | otherwise = countReason code xs

-- Recursive count of adult patients (age >= 18)
countAdults :: [Patient] -> Int
countAdults [] = 0
countAdults ((_,age,_):xs)
    | age >= 18 = 1 + countAdults xs
    | otherwise = countAdults xs

-- Trim whitespace from both ends
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Split string by a predicate (helper)
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen _ "" = []
wordsWhen p s =
    let (w, s') = break p s
    in w : case s' of
        [] -> []
        (_:rest) -> wordsWhen p rest

-- Safe integer read (returns Nothing on failure)
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s :: [(Int, String)] of
    [(n, "")] -> Just n
    _         -> Nothing

-- Safe parse of "Name:Age:Reason" -> Maybe Patient
parseRecord :: String -> Maybe Patient
parseRecord str =
    case wordsWhen (==':') str of
        (nRaw:ageRaw:reasonRaw:_) ->
            let name = trim nRaw
                mAge = safeReadInt (trim ageRaw)
                mReason = safeReadInt (trim reasonRaw)
            in case (mAge, mReason) of
                 (Just age, Just reason) -> Just (name, age, reason)
                 _ -> Nothing
        _ -> Nothing

-- Pretty print the report
printResults :: [Patient] -> IO ()
printResults patients = do
    putStrLn "--------------------------------------"
    putStrLn "   Hospital Patient Records Report"
    putStrLn "--------------------------------------"
    putStrLn $ "-- General Checkups: " ++ show (countReason 1 patients)
    putStrLn $ "-- Emergencies: " ++ show (countReason 2 patients)
    putStrLn $ "-- Surgeries: " ++ show (countReason 3 patients)
    putStrLn "--------------------------------------"
    putStrLn $ "Total Patients: " ++ show (length patients)
    putStrLn $ "Total Adult Patients (18+): " ++ show (countAdults patients)
    putStrLn "--------------------------------------"
    putStrLn "End of Report"

-- Main: if stdin empty or exactly "NIL" -> sampleData; else parse stdin
main :: IO ()
main = do
    contents <- getContents
    -- collect non-empty trimmed lines
    let rawLines = lines contents
        nonEmptyTrimmed = [ trim l | l <- rawLines, not (all isSpace l) ]
        isNilOnly = case nonEmptyTrimmed of
                      [s] -> map toUpper s == "NIL"
                      _   -> False
    if null nonEmptyTrimmed || isNilOnly
       then do
           putStrLn "(Using built-in sample data)"
           printResults sampleData
       else do
           let parsed = [ p | Just p <- map parseRecord nonEmptyTrimmed ]
           -- Note: malformed lines are skipped silently
           printResults parsed
