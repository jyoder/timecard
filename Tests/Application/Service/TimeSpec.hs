module Tests.Application.Service.TimeSpec where

import Application.Service.Time
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "parseDay" do
        it "returns the day associated with a string of the format Y-m-d" do
            parseDay "2021-09-13" `shouldBe` Just (toDay "2021-09-13")
        it "returns nothing when the string does not have the form Y-m-d" do
            parseDay "20X21-09-13" `shouldBe` Nothing

    describe "startOfWeek" do
        it "returns the Monday of the week which contains the given Monday" do
            startOfWeek (toDay "2021-09-13") `shouldBe` toDay "2021-09-13"
        it "returns the Monday of the week which contains the given Tuesday" do
            startOfWeek (toDay "2021-09-14") `shouldBe` toDay "2021-09-13"
        it "returns the Monday of the week which contains the given Wednesday" do
            startOfWeek (toDay "2021-09-15") `shouldBe` toDay "2021-09-13"
        it "returns the Monday of the week which contains the given Thursday" do
            startOfWeek (toDay "2021-09-16") `shouldBe` toDay "2021-09-13"
        it "returns the Monday of the week which contains the given Friday" do
            startOfWeek (toDay "2021-09-17") `shouldBe` toDay "2021-09-13"
        it "returns the Monday of the week which contains the given Saturday" do
            startOfWeek (toDay "2021-09-18") `shouldBe` toDay "2021-09-13"
        it "returns the Monday of the week which contains the given Sunday" do
            startOfWeek (toDay "2021-09-19") `shouldBe` toDay "2021-09-13"

    describe "nextWorkingDay" do
        it "returns Monday if the given day is Sunday" do
            nextWorkingDay (toDay "2021-09-12") `shouldBe` toDay "2021-09-13"
        it "returns Tuesday if the given day is Monday" do
            nextWorkingDay (toDay "2021-09-13") `shouldBe` toDay "2021-09-14"
        it "returns Wednesday if the given day is Tuesday" do
            nextWorkingDay (toDay "2021-09-14") `shouldBe` toDay "2021-09-15"
        it "returns Thursday if the given day is Wednesday" do
            nextWorkingDay (toDay "2021-09-15") `shouldBe` toDay "2021-09-16"
        it "returns Friday if the given day is Thursday" do
            nextWorkingDay (toDay "2021-09-16") `shouldBe` toDay "2021-09-17"
        it "returns Monday if the given day is Friday" do
            nextWorkingDay (toDay "2021-09-17") `shouldBe` toDay "2021-09-20"
        it "returns Monday if the given day is Saturday" do
            nextWorkingDay (toDay "2021-09-18") `shouldBe` toDay "2021-09-20"