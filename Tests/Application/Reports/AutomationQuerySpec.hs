module Tests.Application.Reports.AutomationQuerySpec where

import qualified Application.Reports.AutomationQuery as Reports.AutomationQuery
import Generated.Types
import IHP.ControllerPrelude
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "fetch" do
            itIO "returns rows to indicate the automation status of each crew member each day" do
                user <-
                    newRecord @User
                        |> set #email "nar@whal.com"
                        |> set #passwordHash "password"
                        |> createRecord

                person1 <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Builder"
                        |> set #goesBy "Bob the Builder"
                        |> createRecord

                workerSetting1 <-
                    newRecord @WorkerSetting
                        |> set #personId (get #id person1)
                        |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                        |> set #isActive True
                        |> createRecord

                phoneNumber1 <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                phoneContact1 <-
                    newRecord @PhoneContact
                        |> set #personId (get #id person1)
                        |> set #phoneNumberId (get #id phoneNumber1)
                        |> createRecord

                auditEntry1 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:00 PDT")
                        |> set #phoneNumberId (get #id phoneNumber1)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 1"
                        |> createRecord

                auditEntry2 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:01 PDT")
                        |> set #phoneNumberId (get #id phoneNumber1)
                        |> set #action MessageReceived
                        |> set #actionContext "Entry 2"
                        |> createRecord

                phoneNumber2 <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                person2 <-
                    newRecord @Person
                        |> set #firstName "Rob"
                        |> set #lastName "Destroyer"
                        |> set #goesBy "Rob the Destroyer"
                        |> createRecord

                workerSetting2 <-
                    newRecord @WorkerSetting
                        |> set #personId (get #id person2)
                        |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                        |> set #isActive True
                        |> createRecord

                phoneContact2 <-
                    newRecord @PhoneContact
                        |> set #personId (get #id person2)
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> createRecord

                auditEntry3 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:02 PDT")
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> set #userId (Just $ get #id user)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 3"
                        |> createRecord

                auditEntry4 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-31 07:00:02 PDT")
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 4"
                        |> createRecord

                rows <- Reports.AutomationQuery.fetch Reports.AutomationQuery.ByDay (toDay "2021-10-30")
                rows
                    `shouldBe` [ Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-30"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.FullyAutomated
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-29"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-28"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-27"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-26"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-25"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-30"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-29"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-28"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-27"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-26"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-25"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               ]

            itIO "returns rows to indicate the automation status of each crew member each week" do
                user <-
                    newRecord @User
                        |> set #email "nar@whal.com"
                        |> set #passwordHash "password"
                        |> createRecord

                person1 <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Builder"
                        |> set #goesBy "Bob the Builder"
                        |> createRecord

                workerSetting1 <-
                    newRecord @WorkerSetting
                        |> set #personId (get #id person1)
                        |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                        |> set #isActive True
                        |> createRecord

                phoneNumber1 <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                phoneContact1 <-
                    newRecord @PhoneContact
                        |> set #personId (get #id person1)
                        |> set #phoneNumberId (get #id phoneNumber1)
                        |> createRecord

                auditEntry1 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:00 PDT")
                        |> set #phoneNumberId (get #id phoneNumber1)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 1"
                        |> createRecord

                auditEntry2 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:01 PDT")
                        |> set #phoneNumberId (get #id phoneNumber1)
                        |> set #action MessageReceived
                        |> set #actionContext "Entry 2"
                        |> createRecord

                phoneNumber2 <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                person2 <-
                    newRecord @Person
                        |> set #firstName "Rob"
                        |> set #lastName "Destroyer"
                        |> set #goesBy "Rob the Destroyer"
                        |> createRecord

                workerSetting2 <-
                    newRecord @WorkerSetting
                        |> set #personId (get #id person2)
                        |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                        |> set #isActive True
                        |> createRecord

                phoneContact2 <-
                    newRecord @PhoneContact
                        |> set #personId (get #id person2)
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> createRecord

                auditEntry3 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:02 PDT")
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> set #userId (Just $ get #id user)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 3"
                        |> createRecord

                auditEntry4 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-31 07:00:02 PDT")
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 4"
                        |> createRecord

                rows <- Reports.AutomationQuery.fetch Reports.AutomationQuery.ByWeek (toDay "2021-11-08")
                rows
                    `shouldBe` [ Reports.AutomationQuery.Row
                                    { date = toDay "2021-11-08"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-11-01"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-25"
                                    , personId = get #id person1
                                    , personFirstName = "Bob"
                                    , personLastName = "Builder"
                                    , automationStatus = Reports.AutomationQuery.FullyAutomated
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-11-08"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-11-01"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NoActivity
                                    }
                               , Reports.AutomationQuery.Row
                                    { date = toDay "2021-10-25"
                                    , personId = get #id person2
                                    , personFirstName = "Rob"
                                    , personLastName = "Destroyer"
                                    , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                                    }
                               ]

            itIO "only returns rows for active workers" do
                person <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Builder"
                        |> set #goesBy "Bob the Builder"
                        |> createRecord

                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                phoneContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id person)
                        |> set #phoneNumberId (get #id phoneNumber)
                        |> createRecord

                auditEntry <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:00 PDT")
                        |> set #phoneNumberId (get #id phoneNumber)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 1"
                        |> createRecord

                rows <- Reports.AutomationQuery.fetch Reports.AutomationQuery.ByWeek (toDay "2021-11-08")
                rows `shouldBe` []
