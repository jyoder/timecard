module Tests.Application.Service.PdfSpec where

import Application.Service.Pdf
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "loadBootstrap" do
            itIO "loads minified bootstrap into memory" do
                bootstrap <- loadBootstrap
                null bootstrap `shouldBe` False

    describe "bootstrapPath" do
        it "returns the path to the minified bootstrap file" do
            bootstrapPath `shouldBe` "build/ihp-lib/static/vendor/bootstrap.min.css"