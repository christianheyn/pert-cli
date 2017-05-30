isSubstringOf :: [Char] -> [Char] -> Bool
isSubstringOf _ [] = False
isSubstringOf [] _ = False
isSubstringOf (stack:stackRest)(needle:needleRest)
    | stack == needle = True
    | otherwise = (stack:stackRest) `isSubstringOf` needleRest

containsString :: [Char] -> [Char] -> Bool
containsString value str = str == [c | c <- str, c `elem` value]

-- Check units
tellUnit value 
    | value `containsString` "min" = "Minutes"
    | value `containsString` "hour" = "Hours"
    | value `containsString` "day" = "Days"
    | otherwise = "default Hours"

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
