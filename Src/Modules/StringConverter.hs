module Src.Modules.StringConverter (
    filterNumberAndPointsFromString,
    convertStringToFloat,
    rmDuplicateChar
) where

recursiveKeepOnce :: [Char] -> Char -> Bool -> [Char]
recursiveKeepOnce (s:sx) once check =
    let newCheck = if s == once then False else True
        filterRest = [s] ++ filter (\y -> y /= once) sx

    in if check && s /= once
        then
            if length sx > 0
                then [s] ++ recursiveKeepOnce sx once newCheck
                else filterRest
        else filterRest
-- End recursiveKeepOnce

rmDuplicateChar :: [Char] -> Char -> [Char]
rmDuplicateChar charList rmChar = recursiveKeepOnce charList rmChar True


filterNumberAndPointsFromString :: String -> String
filterNumberAndPointsFromString s =
    let allowedString = ['0'..'9'] ++ ['.', ',']
        toPoint n
            | n == ',' = '.'
            | otherwise = n

        allowedStringFiltered = filter (`elem` allowedString) s
        justPoints = map toPoint allowedStringFiltered
        onePoint = rmDuplicateChar justPoints '.'
    in onePoint
-- End filterNumberAndPointsFromString

convertStringToFloat :: String -> Float
convertStringToFloat s =
    let betterString = filterNumberAndPointsFromString s
        saftyZero = "0" ++ betterString
        asFloat = read saftyZero :: Float
    in asFloat
