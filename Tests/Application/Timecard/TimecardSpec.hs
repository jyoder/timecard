module Tests.Application.Timecard.TimecardSpec where

import qualified Application.Timecard.Timecard as Timecard.Timecard
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "validate" do
            itIO "validates that weekOf is the start of the week" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                let timecard =
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-22")
                            |> set #personId (get #id ron)

                timecard <- Timecard.Timecard.validate timecard
                timecard |> get #meta |> get #annotations
                    `shouldBe` [("weekOf", TextViolation "weekOf must be a Monday, the start of the week")]

            itIO "validates that weekOf matches associated timecard entry dates" do
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

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-22")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecard <-
                    timecard
                        |> set #weekOf (toDay "2021-07-12")
                        |> Timecard.Timecard.validate

                timecard |> get #meta |> get #annotations
                    `shouldBe` [("weekOf", TextViolation "weekOf must match timecard entries")]

        describe "fetchOrCreate" do
            itIO "fetches an existing timecard" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                burgie <-
                    newRecord @Person
                        |> set #firstName "Burger"
                        |> set #lastName "King"
                        |> set #goesBy "Burgie"
                        |> createRecord

                ronTimecard1 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                ronTimecard2 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-28")
                        |> set #personId (get #id ron)
                        |> createRecord

                burgieTimecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id burgie)
                        |> createRecord

                timecard <-
                    Timecard.Timecard.fetchOrCreate
                        (get #id ron)
                        (toDay "2021-06-28")

                get #id timecard `shouldBe` get #id ronTimecard2

            itIO "creates a new timecard if a matching one does not yet exist" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                burgie <-
                    newRecord @Person
                        |> set #firstName "Burger"
                        |> set #lastName "King"
                        |> set #goesBy "Burgie"
                        |> createRecord

                ronTimecard1 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                burgieTimecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id burgie)
                        |> createRecord

                ronTimecard2 <-
                    Timecard.Timecard.fetchOrCreate
                        (get #id ron)
                        (toDay "2021-06-28")

                get #personId ronTimecard2 `shouldBe` get #id ron
                get #weekOf ronTimecard2 `shouldBe` toDay "2021-06-28"
                get #id ronTimecard2 `shouldNotBe` get #id ronTimecard1
