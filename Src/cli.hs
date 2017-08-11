import Src.Modules.StringConverter -- > convertStringToFloat
import Src.Modules.UnitHelper -- > convertStringToFloat
import Src.Modules.Calculater -- > convertStringToFloat

-- Main
main = do
    let greenStart = "\x1b[32m"
        yellowStart = "\x1b[33m"
        redStart = "\x1b[31m"
        blueStart = "\x1b[36m"
        colorEnd = "\x1b[0m"

    putStrLn (blueStart ++ "------------------" ++ colorEnd)
    putStrLn (yellowStart ++ "Likely case?" ++ colorEnd)
    likelyInput <- getLine
    putStrLn (yellowStart ++ "Worst case?" ++ redStart)
    worstInput <- getLine
    putStrLn (yellowStart ++ "Best case?" ++ greenStart)
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

        resultInMinutes = estimatePert likelyMinutes worstMinutes bestMinutes
        resultInMinutesString = show resultInMinutes
        resultInHours = onlyTwoDecimalDigits (resultInMinutes / 60)
        resultInHoursString = show resultInHours
        resultInDays = onlyTwoDecimalDigits resultInHours / 8
        resultInDaysString = show resultInDays

    putStrLn (yellowStart ++ ".................." ++ colorEnd)
    putStrLn ("You probably need:")
    putStrLn (greenStart ++ resultInHoursString ++ " Hour(s)" ++ colorEnd)
    putStrLn (yellowStart ++ resultInDaysString ++ " Day(s)" ++ colorEnd)
    putStrLn (blueStart ++ "------------------" ++ colorEnd)
