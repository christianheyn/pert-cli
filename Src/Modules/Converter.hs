module Src.Modules.Converter (
    filterNumberFromString,
    convertStringToFloat,
    rmDuplicatePoints
) where

bothTrue first second
    | first == False = False 
    | second == False = False
    | first == True && second == True = True

rmDuplicatePoints :: String -> String
rmDuplicatePoints str =
    let pointCounter = 0
        moreThanOnePoint :: Char -> Bool
        moreThanOnePoint c = 
            let isPoint = if c == '.' then True else False
                pointCounter = if isPoint == True then 1 else pointCounter
                isZero = if pointCounter == 0 then True else False
            in (bothTrue isPoint isZero)

        filtered = [c | c <- str, moreThanOnePoint c]

    in filtered

filterNumberFromString :: String -> String
filterNumberFromString s =
    let allowedString = ['0'..'9'] ++ ['.', ',']
        toPoint n
            | n == ',' = '.'
            | otherwise = n

        allowedStringFiltered = filter (`elem` allowedString) s
        justPoints = map toPoint allowedStringFiltered
        onePoint = rmDuplicatePoints justPoints
    in onePoint

convertStringToFloat :: String -> Float
convertStringToFloat s =
    let betterString = filterNumberFromString s
        asFloat = read betterString :: Float
    in asFloat
