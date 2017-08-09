containsChars :: [Char] -> [Char] -> Bool
containsChars value str = str == [c | c <- str, c `elem` value]

minutesToHour :: Float -> Float
minutesToHour m = m / (60 / 100)

daysToHours ::  Float -> Float
daysToHours d = d * 8

filterNumberFromString :: String -> String
filterNumberFromString s =
    let allowedString = ['0'..'9'] ++ ['.', ',']
        toPoint n
            | n == ',' = '.'
            | otherwise = n

        f = filter (`elem` allowedString) s
        d = map toPoint f
    in d


convertStringToFloat :: String -> Float
convertStringToFloat s =
    let betterString = filterNumberFromString s
        asFloat = read betterString :: Float
    in asFloat

tellUnit :: String -> String
tellUnit value 
    | value `containsChars` "m" = "Minutes"
    | value `containsChars` "h" = "Hours"
    | value `containsChars` "d" = "Days"
    | otherwise = "Hours"

-- Main
main = do  
    putStrLn "Likely case?"
    likely <- getLine
    putStrLn "Worst case?"
    worst <- getLine
    putStrLn "Best case?"
    best <- getLine

    let l = tellUnit likely
        w = tellUnit worst
        b = tellUnit best


    putStrLn ("Likely case: " ++ l)
    putStrLn ("Worst case: " ++ w)
    putStrLn ("Best case: " ++ b)
    print (convertStringToFloat "15,00")
