isSubstringOf :: [Char] -> [Char] -> Bool
isSubstringOf [][] = False
isSubstringOf _[] = False
isSubstringOf []_ = False
isSubstringOf (stack:stackS)(needle:needleS)
    | stack == needle = True
    | otherwise = (stack:stackS) `isSubstringOf` needleS

-- check units
tellUnit value 
    | "m" `isSubstringOf` value = "Minutes"
    | "h" `isSubstringOf` value = "Hours"
    | "d" `isSubstringOf` value = "Days"
    | otherwise = "default Hours"

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
