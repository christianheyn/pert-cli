import Modules.Converter -- > convertStringToFloat

containsChars :: [Char] -> [Char] -> Bool
containsChars value str = str == [c | c <- str, c `elem` value]

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

        likelyMinutes = getMinutesFrom likelyFloat likelyUnit
        worstMinutes = getMinutesFrom worstFloat worstUnit
        bestMinutes = getMinutesFrom bestFloat bestUnit

        sumMinutes = (bestMinutes + (likelyMinutes * 4) + worstMinutes)

    putStrLn ("U probaly need:")
    print (sumMinutes / 6 / 60 )
