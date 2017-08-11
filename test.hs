import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Src.Pert (estimate)
import Src.Modules.StringConverter (
        convertStringToFloat,
        filterNumberAndPointsFromString,
        rmDuplicateChar
    )
import Src.Modules.UnitHelper (
        containsString,
        getUnit,
        getMinutesFrom
    )
import Src.Modules.Calculater (
        onlyTwoDecimalDigits,
        estimatePert
    )

main :: IO ()
main = hspec $ do
    describe "* Src.Modules.StringConverter" $ do
        -- convertStringToFloat
        describe "convertStringToFloat" $ do
            it "convert a string to float" $ do
                convertStringToFloat "4" `shouldBe` 4.0
                convertStringToFloat "ww4eddd" `shouldBe` 4.0
            it "interpretes a ',' as '.'" $ do
                convertStringToFloat "004,9h" `shouldBe` 4.9
            it "interpretes only first '.' or ','" $ do
                convertStringToFloat "004,9h" `shouldBe` 4.9
            it "interpretes shorthand case like '.5' to 0.5" $ do
                convertStringToFloat ".5 Days" `shouldBe` 0.5
                convertStringToFloat ".5.5 Days" `shouldBe` 0.55
                convertStringToFloat "5.5 Days" `shouldBe` 5.5

        -- filterNumberAndPointsFromString
        describe "filterNumberAndPointsFromString" $ do
            it "removes all non-numeric charakters" $ do
                filterNumberAndPointsFromString "4ee" `shouldBe` "4"
                filterNumberAndPointsFromString "a1b2" `shouldBe` "12"
            it "convert ',' to ." $ do
                filterNumberAndPointsFromString "4,3" `shouldBe` "4.3"
            it "removes duplicated '.' - keeps the first '.'" $ do
                filterNumberAndPointsFromString "4,3.9" `shouldBe` "4.39"

        -- rmDuplicateChar
        describe "rmDuplicateChar" $ do
            it "removes all duplicated '.'" $ do
                rmDuplicateChar "lala.lala" '.' `shouldBe` "lala.lala"
                rmDuplicateChar "lala.la.la" '.' `shouldBe` "lala.lala"
                rmDuplicateChar "..lala.la...la" '.' `shouldBe` ".lalalala"
                rmDuplicateChar "lll" 'l' `shouldBe` "l"
                rmDuplicateChar "l" 'l' `shouldBe` "l"

    describe "* Src.Modules.UnitHelper" $ do
        -- convertStringToFloat
        describe "containsString value str" $ do
            it "returns True when value is found in str" $ do
                containsString "45h" "h" `shouldBe` True
                containsString "45h" "4" `shouldBe` True

        -- getUnit
        describe "getUnit value" $ do
            it "returns 'Minutes', 'Hours' or 'Days' when value contains 'm','h' or 'd'" $ do
                getUnit "45h" `shouldBe` "Hours"
                getUnit "h45" `shouldBe` "Hours"
                getUnit "4m" `shouldBe` "Minutes"
                getUnit "m45" `shouldBe` "Minutes"
                getUnit "1d" `shouldBe` "Days"
                getUnit "d.9" `shouldBe` ['D', 'a', 'y', 's']

    describe "* Src.Modules.Calculater" $ do
        -- onlyTwoDecimalDigits
        describe "onlyTwoDecimalDigits n" $ do
            it "returns Float" $ do
                onlyTwoDecimalDigits 2 `shouldBe` 2
            it "calculates Pert and shows only 2 digits" $ do
                onlyTwoDecimalDigits 2.88888 `shouldBe` 2.89

        -- estimatePert
        describe "estimatePert likely worst best" $ do
            it "returns Float" $ do
                estimatePert 2 2 2 `shouldBe` 2
            it "calculates Pert and shows only 2 digits" $ do
                estimatePert 4 8 2 `shouldBe` 4.33
