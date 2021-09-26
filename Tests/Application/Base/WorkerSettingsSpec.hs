module Tests.Application.Base.WorkerSettingsSpec where

import qualified Application.Base.WorkerSettings as WorkerSettings
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "toLanguage" do
        it "returns English when 'english' is specified" do
            WorkerSettings.toLanguage WorkerSettings.english `shouldBe` WorkerSettings.English
        it "returns Spanish when 'spanish' is specified" do
            WorkerSettings.toLanguage WorkerSettings.spanish `shouldBe` WorkerSettings.Spanish
        it "returns English when the language is not recognized" do
            WorkerSettings.toLanguage "unrecognized" `shouldBe` WorkerSettings.English

    describe "english" do
        it "is 'english'" do
            WorkerSettings.english `shouldBe` "english"

    describe "spanish" do
        it "is 'spanish'" do
            WorkerSettings.spanish `shouldBe` "spanish"