module Tests.Application.Timecard.QuerySpec where

import qualified Application.Timecard.Query as Timecard.Query
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "fetchByPerson" do
            itIO "selects only timecard entries associated with the specified person" do
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
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                burgieTimecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id burgieTimecard)
                        |> set #date (toDay "2021-06-24")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                rows <-
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateDescending
                        (get #id ron)

                get #timecardEntryId <$> rows
                    `shouldBe` [ get #id ronTimecardEntry
                               ]

            itIO "sorts rows properly based on the given entry sort criteria" do
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
                        |> set #createdAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry2 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-24")
                        |> set #createdAt (toUtc "2021-06-24 15:00:00 PDT")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry3 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-23")
                        |> set #createdAt (toUtc "2021-06-22 15:00:00 PDT")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                rowsDescending <-
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateDescending
                        (get #id ron)

                get #timecardEntryId <$> rowsDescending
                    `shouldBe` [ get #id timecardEntry2
                               , get #id timecardEntry1
                               , get #id timecardEntry3
                               ]

                rowsAscending <-
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateAscending
                        (get #id ron)

                get #timecardEntryId <$> rowsAscending
                    `shouldBe` [ get #id timecardEntry3
                               , get #id timecardEntry1
                               , get #id timecardEntry2
                               ]

            itIO "sorts rows in descending order by week of timecard" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard1 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecard2 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-28")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry1 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard1)
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry2 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard2)
                        |> set #date (toDay "2021-06-30")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                rowsDescending <-
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateAscending
                        (get #id ron)

                get #timecardEntryId <$> rowsDescending
                    `shouldBe` [ get #id timecardEntry2
                               , get #id timecardEntry1
                               ]

            itIO "limits results to the 4 most recent timecards" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard1 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecard2 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-28")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecard3 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-07-05")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecard4 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-07-12")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecard5 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-07-19")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry1 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard1)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry2 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard2)
                        |> set #date (toDay "2021-06-28")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry3 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard3)
                        |> set #date (toDay "2021-07-05")
                        |> set #jobName "Carl's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry4 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard4)
                        |> set #date (toDay "2021-07-12")
                        |> set #jobName "Wendy's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry5 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard5)
                        |> set #date (toDay "2021-07-19")
                        |> set #jobName "Taco Bell"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                rowsDescending <-
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateAscending
                        (get #id ron)

                get #timecardEntryId <$> rowsDescending
                    `shouldBe` [ get #id timecardEntry5
                               , get #id timecardEntry4
                               , get #id timecardEntry3
                               , get #id timecardEntry2
                               ]

            itIO "includes access token columns when an access token is present" do
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
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
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
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateDescending
                        (get #id ron)

                rows
                    `shouldBe` [ Timecard.Query.Row
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
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]

            itIO "includes signing columns when a signing is present" do
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
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
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
                    Timecard.Query.fetchByPerson
                        Timecard.Query.EntriesDateDescending
                        (get #id ron)

                rows
                    `shouldBe` [ Timecard.Query.Row
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
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]

        describe "fetchById" do
            itIO "selects the timecard with the given id" do
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
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                burgieTimecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id burgieTimecard)
                        |> set #date (toDay "2021-06-24")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                rows <-
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id ronTimecard)

                get #timecardEntryId <$> rows
                    `shouldBe` [ get #id ronTimecardEntry
                               ]

            itIO "sorts rows properly based on the given entry sort criteria" do
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
                        |> set #createdAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry2 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-24")
                        |> set #createdAt (toUtc "2021-06-24 15:00:00 PDT")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry3 <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-23")
                        |> set #createdAt (toUtc "2021-06-22 15:00:00 PDT")
                        |> set #jobName "Burger King"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                rowsDescending <-
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id timecard)

                get #timecardEntryId <$> rowsDescending
                    `shouldBe` [ get #id timecardEntry2
                               , get #id timecardEntry1
                               , get #id timecardEntry3
                               ]

                rowsAscending <-
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateAscending
                        (get #id timecard)

                get #timecardEntryId <$> rowsAscending
                    `shouldBe` [ get #id timecardEntry3
                               , get #id timecardEntry1
                               , get #id timecardEntry2
                               ]

            itIO "includes access token columns when an access token is present" do
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
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
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
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id timecard)

                rows
                    `shouldBe` [ Timecard.Query.Row
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
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]

            itIO "prefers a non-revoked token over a revoked token" do
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
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                accessToken1 <-
                    newRecord @AccessToken
                        |> set #value "secret1"
                        |> set #expiresAt (toUtc "2021-06-23 15:29:00 PDT")
                        |> createRecord

                newRecord @TimecardAccessToken
                    |> set #timecardId (get #id timecard)
                    |> set #accessTokenId (get #id accessToken1)
                    |> createRecord

                accessToken2 <-
                    newRecord @AccessToken
                        |> set #value "secret2"
                        |> set #expiresAt (toUtc "2021-06-24 15:29:00 PDT")
                        |> set #isRevoked True
                        |> createRecord

                newRecord @TimecardAccessToken
                    |> set #timecardId (get #id timecard)
                    |> set #accessTokenId (get #id accessToken2)
                    |> createRecord

                rows <-
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id timecard)

                rows
                    `shouldBe` [ Timecard.Query.Row
                                    { timecardId = get #id timecard
                                    , timecardPersonId = get #personId timecard
                                    , timecardWeekOf = get #weekOf timecard
                                    , accessTokenId = Just $ get #id accessToken1
                                    , accessTokenValue = Just $ get #value accessToken1
                                    , accessTokenExpiresAt = Just $ get #expiresAt accessToken1
                                    , accessTokenIsRevoked = Just $ get #isRevoked accessToken1
                                    , signingId = Nothing
                                    , signingSignedAt = Nothing
                                    , timecardEntryId = get #id timecardEntry
                                    , timecardEntryDate = get #date timecardEntry
                                    , timecardEntryJobName = get #jobName timecardEntry
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]

            itIO "prefers the token with a later expiration time" do
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
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                accessToken1 <-
                    newRecord @AccessToken
                        |> set #value "secret1"
                        |> set #expiresAt (toUtc "2021-06-23 15:29:00 PDT")
                        |> createRecord

                newRecord @TimecardAccessToken
                    |> set #timecardId (get #id timecard)
                    |> set #accessTokenId (get #id accessToken1)
                    |> createRecord

                accessToken2 <-
                    newRecord @AccessToken
                        |> set #value "secret2"
                        |> set #expiresAt (toUtc "2021-06-24 15:29:00 PDT")
                        |> createRecord

                newRecord @TimecardAccessToken
                    |> set #timecardId (get #id timecard)
                    |> set #accessTokenId (get #id accessToken2)
                    |> createRecord

                rows <-
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id timecard)

                rows
                    `shouldBe` [ Timecard.Query.Row
                                    { timecardId = get #id timecard
                                    , timecardPersonId = get #personId timecard
                                    , timecardWeekOf = get #weekOf timecard
                                    , accessTokenId = Just $ get #id accessToken2
                                    , accessTokenValue = Just $ get #value accessToken2
                                    , accessTokenExpiresAt = Just $ get #expiresAt accessToken2
                                    , accessTokenIsRevoked = Just $ get #isRevoked accessToken2
                                    , signingId = Nothing
                                    , signingSignedAt = Nothing
                                    , timecardEntryId = get #id timecardEntry
                                    , timecardEntryDate = get #date timecardEntry
                                    , timecardEntryJobName = get #jobName timecardEntry
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]

            itIO "includes signing columns when a signing is present" do
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
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> set #hoursWorked 8.0
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
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id timecard)

                rows
                    `shouldBe` [ Timecard.Query.Row
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
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]

            itIO "prefers newly created signings" do
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
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> set #hoursWorked 8.0
                        |> createRecord

                signing1 <-
                    newRecord @Signing
                        |> set #name "Ron McDonald"
                        |> set #createdAt (toUtc "2021-06-20 15:29:00 PDT")
                        |> set #signedAt (toUtc "2021-06-23 15:29:00 PDT")
                        |> set #ipAddress "127.0.0.1"
                        |> createRecord

                newRecord @TimecardSigning
                    |> set #timecardId (get #id timecard)
                    |> set #signingId (get #id signing1)
                    |> createRecord

                signing2 <-
                    newRecord @Signing
                        |> set #name "Ron McDonald"
                        |> set #createdAt (toUtc "2021-06-21 15:29:00 PDT")
                        |> set #signedAt (toUtc "2021-06-23 15:29:00 PDT")
                        |> set #ipAddress "127.0.0.1"
                        |> createRecord

                newRecord @TimecardSigning
                    |> set #timecardId (get #id timecard)
                    |> set #signingId (get #id signing2)
                    |> createRecord

                rows <-
                    Timecard.Query.fetchById
                        Timecard.Query.EntriesDateDescending
                        (get #id timecard)

                rows
                    `shouldBe` [ Timecard.Query.Row
                                    { timecardId = get #id timecard
                                    , timecardPersonId = get #personId timecard
                                    , timecardWeekOf = get #weekOf timecard
                                    , accessTokenId = Nothing
                                    , accessTokenValue = Nothing
                                    , accessTokenExpiresAt = Nothing
                                    , accessTokenIsRevoked = Nothing
                                    , signingId = Just $ get #id signing2
                                    , signingSignedAt = Just $ get #signedAt signing2
                                    , timecardEntryId = get #id timecardEntry
                                    , timecardEntryDate = get #date timecardEntry
                                    , timecardEntryJobName = get #jobName timecardEntry
                                    , timecardEntryClockedInAt = get #clockedInAt timecardEntry
                                    , timecardEntryClockedOutAt = get #clockedOutAt timecardEntry
                                    , timecardEntryLunchDuration = get #lunchDuration timecardEntry
                                    , timecardEntryHoursWorked = get #hoursWorked timecardEntry
                                    , timecardEntryWorkDone = get #workDone timecardEntry
                                    , timecardEntryInvoiceTranslation = get #invoiceTranslation timecardEntry
                                    }
                               ]
