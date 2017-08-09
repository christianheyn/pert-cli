-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Src.Pert -- estimate
import Src.Modules.Converter
    -- convertStringToFloat
    -- filterNumberFromString
    -- rmDuplicatePoints

main :: IO ()
main = hspec $ do
    describe "Converter" $ do
        describe "convertStringToFloat" $ do
            it "convert a string to float" $ do
                convertStringToFloat "4e" `shouldBe` 4.0


        describe "filterNumberFromString" $ do

            it "removes all non-numeric charakters" $ do
                filterNumberFromString "4ee" `shouldBe` "4"
                filterNumberFromString "a1b2" `shouldBe` "12"

            it "convert ',' to ." $ do
                filterNumberFromString "4,3" `shouldBe` "4.3"

            -- it "removes duplicated '.' - keeps the first '.'" $ do
            --     filterNumberFromString "4,3.9" `shouldBe` "4.39"

        describe "rmDuplicatePoints" $ do

            it "removes all duplicated '.'" $ do
                rmDuplicatePoints "lala.lala" `shouldBe` "lala.lala"
                rmDuplicatePoints "lala.la.la" `shouldBe` "lala.lala"
