module Application.Timecard.AccessTokenSpec where

import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "create" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "inserts a timecard access token and associated access token with the correct expiration" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardAccessToken <-
                    Timecard.AccessToken.create
                        (toUtc "2021-06-21 15:30:00 PDT")
                        (get #id timecard)

                accessToken <-
                    fetchOne (get #accessTokenId timecardAccessToken)

                get #expiresAt accessToken
                    `shouldBe` toUtc "2021-06-21 15:30:00 PDT"

    describe "expirationFrom" $ do
        it "returns a time three weeks after the given time" $ do
            Timecard.AccessToken.expirationFrom (toUtc "2021-06-21 15:30:00 PDT")
                `shouldBe` toUtc "2021-07-12 15:30:00 PDT"
