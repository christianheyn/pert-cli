import Src.Modules.Converter -- > convertStringToFloat
import Src.Modules.UnitHelper -- > convertStringToFloat


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
