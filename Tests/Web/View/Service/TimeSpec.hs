module Tests.Web.View.Service.TimeSpec where

import IHP.Prelude
import Test.Hspec
import Tests.Support
import Web.View.Service.Time

spec :: Spec
spec = do
    describe "formatDay" do
        it "returns a string with 'mm/dd/yyyy' format" do
            formatDay (toDay "2021-08-01") `shouldBe` "08/01/2021"

    describe "formatTimeOfDay" do
        it "returns a string with 'h:mm AM/PM' format" do
            formatTimeOfDay (toTimeOfDay "07:03:01") `shouldBe` "7:03 AM"

    describe "formatDateTime" do
        it "returns a string with 'yyyy-mm-ddThh:mm:ss+tz' format" do
            formatDateTime (toUtc "2021-06-01 15:00:00 PDT") `shouldBe` "2021-06-01T22:00:00+0000"
