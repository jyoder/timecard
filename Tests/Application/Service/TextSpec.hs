module Tests.Application.Service.TextSpec where

import Application.Service.Text
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "isBlank" do
        it "returns true if the string length is zero" do
            isBlank "" `shouldBe` True
        it "returns true if the string contains only whitespace" do
            isBlank "    " `shouldBe` True
        it "returns false if the string contains non-whitespace" do
            isBlank "  a  " `shouldBe` False
