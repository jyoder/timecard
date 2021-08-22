module Tests.Application.Config.EnvironmentSpec where

import qualified Application.Config.Environment as Environment
import qualified Data.Text
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import System.Environment (setEnv)
import System.Random (randomIO)
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "loadEnvVars" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "loads all specified environment variables" do
                tokenId1 <- randomIO :: IO Int
                let tokenName1 = "TOKEN_" <> show (abs tokenId1)

                tokenId2 <- randomIO :: IO Int
                let tokenName2 = "TOKEN_" <> show (abs tokenId2)

                setEnv (Data.Text.unpack tokenName1) "token1"
                setEnv (Data.Text.unpack tokenName2) "token2"

                vars <- Environment.loadEnvVars [tokenName1, tokenName2]
                vars `shouldBe` Right [(tokenName1, "token1"), (tokenName2, "token2")]

            itIO "returns an error message if any of the environment variables was not found" do
                tokenId1 <- randomIO :: IO Int
                let tokenName1 = "TOKEN_" <> show (abs tokenId1)

                tokenId2 <- randomIO :: IO Int
                let tokenName2 = "TOKEN_" <> show (abs tokenId2)

                setEnv (Data.Text.unpack tokenName1) "token1"

                vars <- Environment.loadEnvVars [tokenName1, tokenName2]
                vars `shouldBe` Left ("Error: Missing environment variables: " <> tokenName2)

    describe "findVar" do
        it "returns the name value pair if the name was found" do
            Environment.findVar [("a", "1"), ("b", "2")] "b" `shouldBe` "2"

        it "returns the empty string if the name was not found" do
            Environment.findVar [("a", "1"), ("b", "2")] "c" `shouldBe` ""
