module Tests.Application.Timecard.SigningSpec where

import qualified Application.Timecard.Signing as Timecard.Signing
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "create" do
            itIO "inserts the signing" do
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

                signing <-
                    newRecord @Signing
                        |> set #name "Ronald McDonald"
                        |> set #ipAddress "127.0.0.1"
                        |> createRecord

                timecardSigning <-
                    Timecard.Signing.create
                        (get #id timecard)
                        (get #id signing)

                timecardSigning <- fetch (get #id timecardSigning)
                get #timecardId timecardSigning `shouldBe` get #id timecard
                get #signingId timecardSigning `shouldBe` get #id signing
