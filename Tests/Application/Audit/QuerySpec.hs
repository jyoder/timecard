module Tests.Application.Audit.QuerySpec where

import qualified Application.Audit.Query as Audit.Query
import "string-interpolate" Data.String.Interpolate (i)
import Generated.Types
import IHP.ControllerPrelude
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "fetchByPhoneNumber" do
            itIO "selects timecard entries associated with a phone number in descending order of creation" do
                phoneNumber1 <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
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

                auditEntry2 <-
                    newRecord @AuditEntry
                        |> set #createdAt (toUtc "2021-10-30 07:00:02 PDT")
                        |> set #phoneNumberId (get #id phoneNumber2)
                        |> set #action MessageSent
                        |> set #actionContext "Entry 3"
                        |> createRecord

                rows <- Audit.Query.fetchByPhoneNumber $ get #id phoneNumber1
                get #actionContext <$> rows `shouldBe` ["Entry 2", "Entry 1"]

            itIO "limits the number of rows retrieved to 100" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let phoneNumberId = show $ get #id phoneNumber
                    insertTuples = map (\sid -> [i| ('#{phoneNumberId}', NULL, 'message_sent', 'hi mom') |]) [1 .. 101]
                    insertStatement =
                        [i|
                            insert into 
                                audit_entries
                                    (phone_number_id, user_id, "action", action_context) 
                                values 
                                    #{intercalate ",\n" insertTuples};
                        |]

                sqlExec insertStatement ()

                rows <- Audit.Query.fetchByPhoneNumber $ get #id phoneNumber
                length rows `shouldBe` 100