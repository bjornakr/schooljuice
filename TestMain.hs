import Test.Hspec
import SchoolJuice

main = hspec $ do
    describe "Test configuration" $ do
        it "is correctly set up" $ do
            True `shouldBe` True

    describe "findTheX" $ do
        it "returns Nothing when no x is found" $ do
            findTheX [] `shouldBe` Nothing