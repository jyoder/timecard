module Application.Timecard.QueriesSpec where

import qualified Application.Timecard.Queries as Timecard.Queries
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support
import Text.Read (read)

spec :: Spec
spec = do
    describe "fetchRowsByPerson" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            it "selects only timecard entries associated with the specified person" $ withContext do
                withTransactionRollback do
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

                    ronTimecard <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
                            |> set #personId (get #id ron)
                            |> createRecord

                    burgieTimecard <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
                            |> set #personId (get #id burgie)
                            |> createRecord

                    ronTimecardEntry <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id ronTimecard)
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    burgieTimecardEntry <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id burgieTimecard)
                            |> set #date (toDay "2021-06-24")
                            |> set #jobName "Burger King"
                            |> createRecord

                    rows <-
                        Timecard.Queries.fetchRowsByPerson
                            Timecard.Queries.EntriesDateDescending
                            (get #id ron)

                    get #timecardEntryJobName <$> rows
                        `shouldBe` [ "McDonald's" :: Text
                                   ]

            it "sorts rows properly based on the given sort criteria" $ withContext do
                withTransactionRollback do
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

                    timecardEntry1 <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    timecardEntry2 <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-24")
                            |> set #jobName "Burger King"
                            |> createRecord

                    rowsDescending <-
                        Timecard.Queries.fetchRowsByPerson
                            Timecard.Queries.EntriesDateDescending
                            (get #id ron)

                    get #timecardEntryDate <$> rowsDescending
                        `shouldBe` [ toDay "2021-06-24"
                                   , toDay "2021-06-23"
                                   ]

                    rowsAscending <-
                        Timecard.Queries.fetchRowsByPerson
                            Timecard.Queries.EntriesDateAscending
                            (get #id ron)

                    get #timecardEntryDate <$> rowsAscending
                        `shouldBe` [ toDay "2021-06-23"
                                   , toDay "2021-06-24"
                                   ]

            it "includes access token columns when an access token is present" $ withContext do
                withTransactionRollback do
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
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    accessToken <-
                        newRecord @AccessToken
                            |> set #value "secret"
                            |> set #expiresAt (toUtc "2021-06-23 15:29:00 PDT")
                            |> createRecord

                    newRecord @TimecardAccessToken
                        |> set #timecardId (get #id timecard)
                        |> set #accessTokenId (get #id accessToken)
                        |> createRecord

                    rows <-
                        Timecard.Queries.fetchRowsByPerson
                            Timecard.Queries.EntriesDateDescending
                            (get #id ron)

                    rows
                        `shouldBe` [ Timecard.Queries.Row
                                        { timecardId = get #id timecard
                                        , timecardPersonId = get #personId timecard
                                        , timecardWeekOf = get #weekOf timecard
                                        , accessTokenId = Just $ get #id accessToken
                                        , accessTokenValue = Just $ get #value accessToken
                                        , accessTokenExpiresAt = Just $ get #expiresAt accessToken
                                        , accessTokenIsRevoked = Just $ get #isRevoked accessToken
                                        , signingId = Nothing
                                        , signingSignedAt = Nothing
                                        , timecardEntryId = get #id timecardEntry
                                        , timecardEntryDate = get #date timecardEntry
                                        , timecardEntryJobName = get #jobName timecardEntry
                                        , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                        , timecardEntryWorkDone = get #workDone timecardEntry
                                        , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                        }
                                   ]

            it "includes signing columns when a signing is present" $ withContext do
                withTransactionRollback do
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
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    signing <-
                        newRecord @Signing
                            |> set #name "Ron McDonald"
                            |> set #signedAt (toUtc "2021-06-23 15:29:00 PDT")
                            |> set #ipAddress "127.0.0.1"
                            |> createRecord

                    newRecord @TimecardSigning
                        |> set #timecardId (get #id timecard)
                        |> set #signingId (get #id signing)
                        |> createRecord

                    rows <-
                        Timecard.Queries.fetchRowsByPerson
                            Timecard.Queries.EntriesDateDescending
                            (get #id ron)

                    rows
                        `shouldBe` [ Timecard.Queries.Row
                                        { timecardId = get #id timecard
                                        , timecardPersonId = get #personId timecard
                                        , timecardWeekOf = get #weekOf timecard
                                        , accessTokenId = Nothing
                                        , accessTokenValue = Nothing
                                        , accessTokenExpiresAt = Nothing
                                        , accessTokenIsRevoked = Nothing
                                        , signingId = Just $ get #id signing
                                        , signingSignedAt = Just $ get #signedAt signing
                                        , timecardEntryId = get #id timecardEntry
                                        , timecardEntryDate = get #date timecardEntry
                                        , timecardEntryJobName = get #jobName timecardEntry
                                        , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                        , timecardEntryWorkDone = get #workDone timecardEntry
                                        , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                        }
                                   ]

    describe "fetchById" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            it "selects the timecard with the given id" $ withContext do
                withTransactionRollback do
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

                    ronTimecard <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
                            |> set #personId (get #id ron)
                            |> createRecord

                    burgieTimecard <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
                            |> set #personId (get #id burgie)
                            |> createRecord

                    ronTimecardEntry <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id ronTimecard)
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    burgieTimecardEntry <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id burgieTimecard)
                            |> set #date (toDay "2021-06-24")
                            |> set #jobName "Burger King"
                            |> createRecord

                    rows <-
                        Timecard.Queries.fetchRowsById
                            Timecard.Queries.EntriesDateDescending
                            (get #id ronTimecard)

                    get #timecardEntryJobName <$> rows
                        `shouldBe` [ "McDonald's" :: Text
                                   ]

            it "sorts rows properly based on the given sort criteria" $ withContext do
                withTransactionRollback do
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

                    timecardEntry1 <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    timecardEntry2 <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-24")
                            |> set #jobName "Burger King"
                            |> createRecord

                    rowsDescending <-
                        Timecard.Queries.fetchRowsById
                            Timecard.Queries.EntriesDateDescending
                            (get #id timecard)

                    get #timecardEntryDate <$> rowsDescending
                        `shouldBe` [ toDay "2021-06-24"
                                   , toDay "2021-06-23"
                                   ]

                    rowsAscending <-
                        Timecard.Queries.fetchRowsById
                            Timecard.Queries.EntriesDateAscending
                            (get #id timecard)

                    get #timecardEntryDate <$> rowsAscending
                        `shouldBe` [ toDay "2021-06-23"
                                   , toDay "2021-06-24"
                                   ]

            it "includes access token columns when an access token is present" $ withContext do
                withTransactionRollback do
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
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    accessToken <-
                        newRecord @AccessToken
                            |> set #value "secret"
                            |> set #expiresAt (toUtc "2021-06-23 15:29:00 PDT")
                            |> createRecord

                    newRecord @TimecardAccessToken
                        |> set #timecardId (get #id timecard)
                        |> set #accessTokenId (get #id accessToken)
                        |> createRecord

                    rows <-
                        Timecard.Queries.fetchRowsById
                            Timecard.Queries.EntriesDateDescending
                            (get #id timecard)

                    rows
                        `shouldBe` [ Timecard.Queries.Row
                                        { timecardId = get #id timecard
                                        , timecardPersonId = get #personId timecard
                                        , timecardWeekOf = get #weekOf timecard
                                        , accessTokenId = Just $ get #id accessToken
                                        , accessTokenValue = Just $ get #value accessToken
                                        , accessTokenExpiresAt = Just $ get #expiresAt accessToken
                                        , accessTokenIsRevoked = Just $ get #isRevoked accessToken
                                        , signingId = Nothing
                                        , signingSignedAt = Nothing
                                        , timecardEntryId = get #id timecardEntry
                                        , timecardEntryDate = get #date timecardEntry
                                        , timecardEntryJobName = get #jobName timecardEntry
                                        , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                        , timecardEntryWorkDone = get #workDone timecardEntry
                                        , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                        }
                                   ]

            it "includes signing columns when a signing is present" $ withContext do
                withTransactionRollback do
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
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    signing <-
                        newRecord @Signing
                            |> set #name "Ron McDonald"
                            |> set #signedAt (toUtc "2021-06-23 15:29:00 PDT")
                            |> set #ipAddress "127.0.0.1"
                            |> createRecord

                    newRecord @TimecardSigning
                        |> set #timecardId (get #id timecard)
                        |> set #signingId (get #id signing)
                        |> createRecord

                    rows <-
                        Timecard.Queries.fetchRowsById
                            Timecard.Queries.EntriesDateDescending
                            (get #id timecard)

                    rows
                        `shouldBe` [ Timecard.Queries.Row
                                        { timecardId = get #id timecard
                                        , timecardPersonId = get #personId timecard
                                        , timecardWeekOf = get #weekOf timecard
                                        , accessTokenId = Nothing
                                        , accessTokenValue = Nothing
                                        , accessTokenExpiresAt = Nothing
                                        , accessTokenIsRevoked = Nothing
                                        , signingId = Just $ get #id signing
                                        , signingSignedAt = Just $ get #signedAt signing
                                        , timecardEntryId = get #id timecardEntry
                                        , timecardEntryDate = get #date timecardEntry
                                        , timecardEntryJobName = get #jobName timecardEntry
                                        , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                        , timecardEntryWorkDone = get #workDone timecardEntry
                                        , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                        }
                                   ]

toUtc :: String -> UTCTime
toUtc time = zonedTimeToUTC (read time :: ZonedTime)

toDay :: String -> Day
toDay = read
