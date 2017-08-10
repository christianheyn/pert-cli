module Src.Modules.UnitHelper (
    containsString,
    getUnit,
    getMinutesFrom
) where

containsString :: [Char] -> [Char] -> Bool
containsString value str = str == [c | c <- str, c `elem` value]

getUnit :: String -> String
getUnit value
    | value `containsString` "m" = "Minutes"
    | value `containsString` "h" = "Hours"
    | value `containsString` "d" = "Days"
    | otherwise = "Hours"

getMinutesFrom :: Float -> String -> Float
getMinutesFrom t unit
    | unit == "Hours" = t * 60
    | unit == "Days" = t * 8 * 60
    | otherwise = t
