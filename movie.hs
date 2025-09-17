-- Q2. Cinema Ticket Sales Report
-- ------------------------------
-- Adult:3
-- Child:2
-- Senior:5

import Data.Char (isSpace, toUpper)

type Sale = (String, Int)

-- Ticket prices
ticketPrice :: String -> Int
ticketPrice "Adult"  = 12
ticketPrice "Child"  = 8
ticketPrice "Senior" = 10
ticketPrice _        = 0   -- fallback for invalid

-- Built-in sample data
sampleData :: [Sale]
sampleData =
    [ ("Adult", 5)
    , ("Child", 3)
    , ("Senior", 2)
    , ("Adult", 4)
    , ("Child", 1)
    ]

-- Count total tickets for a given category (recursion + pattern matching)
countCategory :: String -> [Sale] -> Int
countCategory _ [] = 0
countCategory cat ((c,q):xs)
    | c == cat  = q + countCategory cat xs
    | otherwise = countCategory cat xs

-- Calculate revenue (recursion)
calcRevenue :: [Sale] -> Int
calcRevenue [] = 0
calcRevenue ((c,q):xs) =
    q * ticketPrice c + calcRevenue xs

-- Trim whitespace helper
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Split string by delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen _ "" = []
wordsWhen p s =
    let (w, s') = break p s
    in w : case s' of
        [] -> []
        (_:rest) -> wordsWhen p rest

-- Safe integer read
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s :: [(Int, String)] of
    [(n, "")] -> Just n
    _         -> Nothing

-- Parse "Category:Quantity"
parseSale :: String -> Maybe Sale
parseSale str =
    case wordsWhen (==':') str of
        (cRaw:qRaw:_) ->
            let cat = trim cRaw
                mq  = safeReadInt (trim qRaw)
            in case mq of
                 Just q -> Just (cat, q)
                 _      -> Nothing
        _ -> Nothing

-- Print report
printReport :: [Sale] -> IO ()
printReport sales = do
    let adultCount  = countCategory "Adult" sales
    let childCount  = countCategory "Child" sales
    let seniorCount = countCategory "Senior" sales
    let totalRev    = calcRevenue sales
    putStrLn "--------------------------------------"
    putStrLn "   Cinema Ticket Sales Report"
    putStrLn "--------------------------------------"
    putStrLn $ "-- Adult Tickets: " ++ show adultCount ++ " (Revenue: $" ++ show (adultCount * 12) ++ ")"
    putStrLn $ "-- Child Tickets: " ++ show childCount ++ " (Revenue: $" ++ show (childCount * 8) ++ ")"
    putStrLn $ "-- Senior Tickets: " ++ show seniorCount ++ " (Revenue: $" ++ show (seniorCount * 10) ++ ")"
    putStrLn "--------------------------------------"
    putStrLn $ "Total Tickets Sold: " ++ show (adultCount + childCount + seniorCount)
    putStrLn $ "Total Revenue: $" ++ show totalRev
    putStrLn "--------------------------------------"
    putStrLn "End of Report"

-- Main: stdin = NIL → sampleData; else → parse input
main :: IO ()
main = do
    contents <- getContents
    let rawLines = lines contents
        nonEmptyTrimmed = [ trim l | l <- rawLines, not (all isSpace l) ]
        isNilOnly = case nonEmptyTrimmed of
                      [s] -> map toUpper s == "NIL"
                      _   -> False
    if null nonEmptyTrimmed || isNilOnly
       then do
           putStrLn "(Using built-in sample data)"
           printReport sampleData
       else do
           let parsed = [ s | Just s <- map parseSale nonEmptyTrimmed ]
           printReport parsed
