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

getUnit :: String -> String
getUnit value 
    | value `containsChars` "m" = "Minutes"
    | value `containsChars` "h" = "Hours"
    | value `containsChars` "d" = "Days"
    | otherwise = "Hours"

getMinutesFrom :: Float -> String -> Float
getMinutesFrom t unit
    | unit == "Hours" = t * 60
    | unit == "Days" = t * 8 * 60
    | otherwise = t

-- Main
main = do  
    print "Likely case?"
    likelyInput <- getLine
    print "Worst case?"
    worstInput <- getLine
    print "Best case?"
    bestInput <- getLine

    let likelyUnit = getUnit likelyInput
        worstUnit = getUnit worstInput
        bestUnit = getUnit bestInput
        likelyFloat = convertStringToFloat likelyInput
        worstFloat = convertStringToFloat worstInput
        bestFloat = convertStringToFloat bestInput

    putStrLn ("Likely case: " ++ likelyUnit)
    print (getMinutesFrom 2 "Days")
