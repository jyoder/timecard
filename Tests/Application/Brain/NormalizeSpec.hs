module Tests.Application.Brain.NormalizeSpec where

import qualified Application.Brain.Normalize as Normalize
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "clockedInAt" do
        context "one digit" do
            it "interprets '1' as 1:00 PM" do
                Normalize.clockedInAt "1" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '2' as 2:00 PM" do
                Normalize.clockedInAt "2" `shouldBe` Just (toTimeOfDay "14:00:00")
            it "interprets '3' as 3:00 PM" do
                Normalize.clockedInAt "3" `shouldBe` Just (toTimeOfDay "15:00:00")
            it "interprets '4' as 4:00 PM" do
                Normalize.clockedInAt "4" `shouldBe` Just (toTimeOfDay "16:00:00")
            it "interprets '5' as 5:00 PM" do
                Normalize.clockedInAt "5" `shouldBe` Just (toTimeOfDay "17:00:00")
            it "interprets '6' as 6:00 AM" do
                Normalize.clockedInAt "6" `shouldBe` Just (toTimeOfDay "06:00:00")
            it "interprets '7' as 7:00 AM" do
                Normalize.clockedInAt "7" `shouldBe` Just (toTimeOfDay "07:00:00")
            it "interprets '8' as 8:00 AM" do
                Normalize.clockedInAt "8" `shouldBe` Just (toTimeOfDay "08:00:00")
            it "interprets '9' as 9:00 AM" do
                Normalize.clockedInAt "9" `shouldBe` Just (toTimeOfDay "09:00:00")
            it "interprets '0' as nothing" do
                Normalize.clockedInAt "0" `shouldBe` Nothing

        context "two digits" do
            it "interprets '10' as 10:00 AM" do
                Normalize.clockedInAt "10" `shouldBe` Just (toTimeOfDay "10:00:00")
            it "interprets '11' as 11:00 AM" do
                Normalize.clockedInAt "11" `shouldBe` Just (toTimeOfDay "11:00:00")
            it "interprets '12' as 12:00 PM" do
                Normalize.clockedInAt "12" `shouldBe` Just (toTimeOfDay "12:00:00")
            it "interprets '13' as 1:00 PM" do
                Normalize.clockedInAt "13" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '14' as 2:00 PM" do
                Normalize.clockedInAt "14" `shouldBe` Just (toTimeOfDay "14:00:00")
            it "interprets '15' as 3:00 PM" do
                Normalize.clockedInAt "15" `shouldBe` Just (toTimeOfDay "15:00:00")
            it "interprets '16' as 4:00 PM" do
                Normalize.clockedInAt "16" `shouldBe` Just (toTimeOfDay "16:00:00")
            it "interprets '17' as 5:00 PM" do
                Normalize.clockedInAt "17" `shouldBe` Just (toTimeOfDay "17:00:00")
            it "interprets '18' as 6:00 PM" do
                Normalize.clockedInAt "18" `shouldBe` Just (toTimeOfDay "18:00:00")
            it "interprets '19' as 7:00 PM" do
                Normalize.clockedInAt "19" `shouldBe` Just (toTimeOfDay "19:00:00")
            it "interprets '20' as 8:00 PM" do
                Normalize.clockedInAt "20" `shouldBe` Just (toTimeOfDay "20:00:00")
            it "interprets '21' as 9:00 PM" do
                Normalize.clockedInAt "21" `shouldBe` Just (toTimeOfDay "21:00:00")
            it "interprets '22' as 10:00 PM" do
                Normalize.clockedInAt "22" `shouldBe` Just (toTimeOfDay "22:00:00")
            it "interprets '23' as 11:00 PM" do
                Normalize.clockedInAt "23" `shouldBe` Just (toTimeOfDay "23:00:00")
            it "interprets '24' as nothing" do
                Normalize.clockedInAt "24" `shouldBe` Nothing

        context "three digits" do
            it "interprets '130' as 1:30 PM" do
                Normalize.clockedInAt "130" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '230' as 2:30 PM" do
                Normalize.clockedInAt "230" `shouldBe` Just (toTimeOfDay "14:30:00")
            it "interprets '330' as 3:30 PM" do
                Normalize.clockedInAt "330" `shouldBe` Just (toTimeOfDay "15:30:00")
            it "interprets '430' as 4:30 PM" do
                Normalize.clockedInAt "430" `shouldBe` Just (toTimeOfDay "16:30:00")
            it "interprets '530' as 5:30 PM" do
                Normalize.clockedInAt "530" `shouldBe` Just (toTimeOfDay "17:30:00")
            it "interprets '630' as 6:30 AM" do
                Normalize.clockedInAt "630" `shouldBe` Just (toTimeOfDay "06:30:00")
            it "interprets '730' as 7:30 AM" do
                Normalize.clockedInAt "730" `shouldBe` Just (toTimeOfDay "07:30:00")
            it "interprets '830' as 8:30 AM" do
                Normalize.clockedInAt "830" `shouldBe` Just (toTimeOfDay "08:30:00")
            it "interprets '930' as 9:30 AM" do
                Normalize.clockedInAt "930" `shouldBe` Just (toTimeOfDay "09:30:00")
            it "interprets '145' as 1:45 PM" do
                Normalize.clockedInAt "145" `shouldBe` Just (toTimeOfDay "13:45:00")
            it "interprets '1:30' as 1:30 PM" do
                Normalize.clockedInAt "1:30" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '030' as nothing" do
                Normalize.clockedInAt "030" `shouldBe` Nothing

        context "four digits (military time)" do
            it "interprets '0130' as 1:30 AM" do
                Normalize.clockedInAt "0130" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '0230' as 2:30 AM" do
                Normalize.clockedInAt "0230" `shouldBe` Just (toTimeOfDay "02:30:00")
            it "interprets '0330' as 3:30 AM" do
                Normalize.clockedInAt "0330" `shouldBe` Just (toTimeOfDay "03:30:00")
            it "interprets '0430' as 4:30 AM" do
                Normalize.clockedInAt "0430" `shouldBe` Just (toTimeOfDay "04:30:00")
            it "interprets '0530' as 5:30 AM" do
                Normalize.clockedInAt "0530" `shouldBe` Just (toTimeOfDay "05:30:00")
            it "interprets '0630' as 6:30 AM" do
                Normalize.clockedInAt "0630" `shouldBe` Just (toTimeOfDay "06:30:00")
            it "interprets '0730' as 7:30 AM" do
                Normalize.clockedInAt "0730" `shouldBe` Just (toTimeOfDay "07:30:00")
            it "interprets '0830' as 8:30 AM" do
                Normalize.clockedInAt "0830" `shouldBe` Just (toTimeOfDay "08:30:00")
            it "interprets '0930' as 1:30 AM" do
                Normalize.clockedInAt "0930" `shouldBe` Just (toTimeOfDay "09:30:00")
            it "interprets '1030' as 10:30 AM" do
                Normalize.clockedInAt "1030" `shouldBe` Just (toTimeOfDay "10:30:00")
            it "interprets '1130' as 11:30 AM" do
                Normalize.clockedInAt "1130" `shouldBe` Just (toTimeOfDay "11:30:00")
            it "interprets '1230' as 12:30 PM" do
                Normalize.clockedInAt "1230" `shouldBe` Just (toTimeOfDay "12:30:00")
            it "interprets '1330' as 1:30 PM" do
                Normalize.clockedInAt "1330" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '1430' as 2:30 PM" do
                Normalize.clockedInAt "1430" `shouldBe` Just (toTimeOfDay "14:30:00")
            it "interprets '1530' as 3:30 PM" do
                Normalize.clockedInAt "1530" `shouldBe` Just (toTimeOfDay "15:30:00")
            it "interprets '1630' as 4:30 PM" do
                Normalize.clockedInAt "1630" `shouldBe` Just (toTimeOfDay "16:30:00")
            it "interprets '1730' as 5:30 PM" do
                Normalize.clockedInAt "1730" `shouldBe` Just (toTimeOfDay "17:30:00")
            it "interprets '1830' as 6:30 PM" do
                Normalize.clockedInAt "1830" `shouldBe` Just (toTimeOfDay "18:30:00")
            it "interprets '1930' as 7:30 PM" do
                Normalize.clockedInAt "1930" `shouldBe` Just (toTimeOfDay "19:30:00")
            it "interprets '2030' as 8:30 PM" do
                Normalize.clockedInAt "2030" `shouldBe` Just (toTimeOfDay "20:30:00")
            it "interprets '2130' as 21:30 PM" do
                Normalize.clockedInAt "2130" `shouldBe` Just (toTimeOfDay "21:30:00")
            it "interprets '2230' as 22:30 PM" do
                Normalize.clockedInAt "2230" `shouldBe` Just (toTimeOfDay "22:30:00")
            it "interprets '2330' as 23:30 PM" do
                Normalize.clockedInAt "2330" `shouldBe` Just (toTimeOfDay "23:30:00")
            it "interprets '01:30' as 1:30 AM" do
                Normalize.clockedInAt "01:30" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '2430' as nothing" do
                Normalize.clockedInAt "2430" `shouldBe` Nothing

        context "digits with am" do
            it "interprets '1a' as 1:00 AM" do
                Normalize.clockedInAt "1a" `shouldBe` Just (toTimeOfDay "01:00:00")
            it "interprets '2a' as 2:00 AM" do
                Normalize.clockedInAt "2a" `shouldBe` Just (toTimeOfDay "02:00:00")
            it "interprets '3a' as 3:00 AM" do
                Normalize.clockedInAt "3a" `shouldBe` Just (toTimeOfDay "03:00:00")
            it "interprets '4a' as 4:00 AM" do
                Normalize.clockedInAt "4a" `shouldBe` Just (toTimeOfDay "04:00:00")
            it "interprets '5a' as 5:00 AM" do
                Normalize.clockedInAt "5a" `shouldBe` Just (toTimeOfDay "05:00:00")
            it "interprets '6a' as 6:00 AM" do
                Normalize.clockedInAt "6a" `shouldBe` Just (toTimeOfDay "06:00:00")
            it "interprets '7a' as 7:00 AM" do
                Normalize.clockedInAt "7a" `shouldBe` Just (toTimeOfDay "07:00:00")
            it "interprets '8a' as 8:00 AM" do
                Normalize.clockedInAt "8a" `shouldBe` Just (toTimeOfDay "08:00:00")
            it "interprets '9a' as 9:00 AM" do
                Normalize.clockedInAt "9a" `shouldBe` Just (toTimeOfDay "09:00:00")
            it "interprets '10a' as 10:00 AM" do
                Normalize.clockedInAt "10a" `shouldBe` Just (toTimeOfDay "10:00:00")
            it "interprets 11a' as 11:00 AM" do
                Normalize.clockedInAt "11a" `shouldBe` Just (toTimeOfDay "11:00:00")
            it "interprets '12a' as 12:00 AM" do
                Normalize.clockedInAt "12a" `shouldBe` Just (toTimeOfDay "00:00:00")
            it "interprets '1am' as 1:00 AM" do
                Normalize.clockedInAt "1am" `shouldBe` Just (toTimeOfDay "01:00:00")
            it "interprets '1 am' as 1:00 AM" do
                Normalize.clockedInAt "1 am" `shouldBe` Just (toTimeOfDay "01:00:00")
            it "interprets '1A' as 1:00 AM" do
                Normalize.clockedInAt "1A" `shouldBe` Just (toTimeOfDay "01:00:00")
            it "interprets '1AM' as 1:00 AM" do
                Normalize.clockedInAt "1AM" `shouldBe` Just (toTimeOfDay "01:00:00")
            it "interprets '1 AM' as 1:00 AM" do
                Normalize.clockedInAt "1 AM" `shouldBe` Just (toTimeOfDay "01:00:00")

        context "digits with pm" do
            it "interprets '1p' as 1:00 PM" do
                Normalize.clockedInAt "1p" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '2p' as 2:00 PM" do
                Normalize.clockedInAt "2p" `shouldBe` Just (toTimeOfDay "14:00:00")
            it "interprets '3p' as 3:00 PM" do
                Normalize.clockedInAt "3p" `shouldBe` Just (toTimeOfDay "15:00:00")
            it "interprets '4p' as 4:00 PM" do
                Normalize.clockedInAt "4p" `shouldBe` Just (toTimeOfDay "16:00:00")
            it "interprets '5p' as 5:00 PM" do
                Normalize.clockedInAt "5p" `shouldBe` Just (toTimeOfDay "17:00:00")
            it "interprets '6p' as 6:00 PM" do
                Normalize.clockedInAt "6p" `shouldBe` Just (toTimeOfDay "18:00:00")
            it "interprets '7p' as 7:00 PM" do
                Normalize.clockedInAt "7p" `shouldBe` Just (toTimeOfDay "19:00:00")
            it "interprets '8p' as 8:00 PM" do
                Normalize.clockedInAt "8p" `shouldBe` Just (toTimeOfDay "20:00:00")
            it "interprets '9p' as 9:00 PM" do
                Normalize.clockedInAt "9p" `shouldBe` Just (toTimeOfDay "21:00:00")
            it "interprets '10p' as 10:00 PM" do
                Normalize.clockedInAt "10p" `shouldBe` Just (toTimeOfDay "22:00:00")
            it "interprets 11p' as 11:00 PM" do
                Normalize.clockedInAt "11p" `shouldBe` Just (toTimeOfDay "23:00:00")
            it "interprets '12p' as 12:00 PM" do
                Normalize.clockedInAt "12p" `shouldBe` Just (toTimeOfDay "12:00:00")
            it "interprets '1pm' as 1:00 PM" do
                Normalize.clockedInAt "1pm" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '1 pm' as 1:00 PM" do
                Normalize.clockedInAt "1 pm" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '1P' as 1:00 PM" do
                Normalize.clockedInAt "1P" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '1PM' as 1:00 PM" do
                Normalize.clockedInAt "1PM" `shouldBe` Just (toTimeOfDay "13:00:00")
            it "interprets '1 PM' as 1:00 PM" do
                Normalize.clockedInAt "1 PM" `shouldBe` Just (toTimeOfDay "13:00:00")

        context "multiple digits with am" do
            it "interprets '1:30a' as 1:30 AM" do
                Normalize.clockedInAt "1:30a" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '2:30a' as 2:30 AM" do
                Normalize.clockedInAt "2:30a" `shouldBe` Just (toTimeOfDay "02:30:00")
            it "interprets '3:30a' as 3:30 AM" do
                Normalize.clockedInAt "3:30a" `shouldBe` Just (toTimeOfDay "03:30:00")
            it "interprets '4:30a' as 4:30 AM" do
                Normalize.clockedInAt "4:30a" `shouldBe` Just (toTimeOfDay "04:30:00")
            it "interprets '5:30a' as 5:30 AM" do
                Normalize.clockedInAt "5:30a" `shouldBe` Just (toTimeOfDay "05:30:00")
            it "interprets '6:30a' as 6:30 AM" do
                Normalize.clockedInAt "6:30a" `shouldBe` Just (toTimeOfDay "06:30:00")
            it "interprets '7:30a' as 1:30 AM" do
                Normalize.clockedInAt "7:30a" `shouldBe` Just (toTimeOfDay "07:30:00")
            it "interprets '8:30a' as 8:30 AM" do
                Normalize.clockedInAt "8:30a" `shouldBe` Just (toTimeOfDay "08:30:00")
            it "interprets '9:30a' as 9:30 AM" do
                Normalize.clockedInAt "9:30a" `shouldBe` Just (toTimeOfDay "09:30:00")
            it "interprets '10:30a' as 10:30 AM" do
                Normalize.clockedInAt "10:30a" `shouldBe` Just (toTimeOfDay "10:30:00")
            it "interprets '11:30a' as 11:30 AM" do
                Normalize.clockedInAt "11:30a" `shouldBe` Just (toTimeOfDay "11:30:00")
            it "interprets '12:30a' as 12:30 AM" do
                Normalize.clockedInAt "12:30a" `shouldBe` Just (toTimeOfDay "00:30:00")
            it "interprets '1:30A' as 1:30 AM" do
                Normalize.clockedInAt "1:30A" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '1:30 a' as 1:30 AM" do
                Normalize.clockedInAt "1:30 a" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '1:30 A' as 1:30 AM" do
                Normalize.clockedInAt "1:30 A" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '1:30 am' as 1:30 AM" do
                Normalize.clockedInAt "1:30 am" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '1:30 AM' as 1:30 AM" do
                Normalize.clockedInAt "1:30 AM" `shouldBe` Just (toTimeOfDay "01:30:00")
            it "interprets '01:30 AM' as nothing" do
                Normalize.clockedInAt "01:30 AM" `shouldBe` Nothing
            it "interprets '13:30 AM' as nothing" do
                Normalize.clockedInAt "13:30 AM" `shouldBe` Nothing

        context "multiple digits with pm" do
            it "interprets '1:30p' as 1:30 PM" do
                Normalize.clockedInAt "1:30p" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '2:30p' as 2:30 PM" do
                Normalize.clockedInAt "2:30p" `shouldBe` Just (toTimeOfDay "14:30:00")
            it "interprets '3:30p' as 3:30 PM" do
                Normalize.clockedInAt "3:30p" `shouldBe` Just (toTimeOfDay "15:30:00")
            it "interprets '4:30p' as 4:30 PM" do
                Normalize.clockedInAt "4:30p" `shouldBe` Just (toTimeOfDay "16:30:00")
            it "interprets '5:30p' as 5:30 PM" do
                Normalize.clockedInAt "5:30p" `shouldBe` Just (toTimeOfDay "17:30:00")
            it "interprets '6:30p' as 6:30 PM" do
                Normalize.clockedInAt "6:30p" `shouldBe` Just (toTimeOfDay "18:30:00")
            it "interprets '7:30p' as 1:30 PM" do
                Normalize.clockedInAt "7:30p" `shouldBe` Just (toTimeOfDay "19:30:00")
            it "interprets '8:30p' as 8:30 PM" do
                Normalize.clockedInAt "8:30p" `shouldBe` Just (toTimeOfDay "20:30:00")
            it "interprets '9:30p' as 9:30 PM" do
                Normalize.clockedInAt "9:30p" `shouldBe` Just (toTimeOfDay "21:30:00")
            it "interprets '10:30p' as 10:30 PM" do
                Normalize.clockedInAt "10:30p" `shouldBe` Just (toTimeOfDay "22:30:00")
            it "interprets '11:30p' as 11:30 PM" do
                Normalize.clockedInAt "11:30p" `shouldBe` Just (toTimeOfDay "23:30:00")
            it "interprets '12:30p' as 12:30 PM" do
                Normalize.clockedInAt "12:30p" `shouldBe` Just (toTimeOfDay "12:30:00")
            it "interprets '1:30p' as 1:30 PM" do
                Normalize.clockedInAt "1:30p" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '1:30P' as 1:30 PM" do
                Normalize.clockedInAt "1:30P" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '1:30 p' as 1:30 PM" do
                Normalize.clockedInAt "1:30 p" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '1:30 P' as 1:30 PM" do
                Normalize.clockedInAt "1:30 P" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '1:30 pm' as 1:30 PM" do
                Normalize.clockedInAt "1:30 pm" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '1:30 PM' as 1:30 PM" do
                Normalize.clockedInAt "1:30 PM" `shouldBe` Just (toTimeOfDay "13:30:00")
            it "interprets '01:30 PM' as nothing" do
                Normalize.clockedInAt "01:30 PM" `shouldBe` Nothing
            it "interprets '13:30 PM' as nothing" do
                Normalize.clockedInAt "13:30 PM" `shouldBe` Nothing

        describe "clockedOutAt" do
            context "one digit" do
                it "interprets '1' as 1:00 PM" do
                    Normalize.clockedOutAt "1" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '2' as 2:00 PM" do
                    Normalize.clockedOutAt "2" `shouldBe` Just (toTimeOfDay "14:00:00")
                it "interprets '3' as 3:00 PM" do
                    Normalize.clockedOutAt "3" `shouldBe` Just (toTimeOfDay "15:00:00")
                it "interprets '4' as 4:00 PM" do
                    Normalize.clockedOutAt "4" `shouldBe` Just (toTimeOfDay "16:00:00")
                it "interprets '5' as 5:00 PM" do
                    Normalize.clockedOutAt "5" `shouldBe` Just (toTimeOfDay "17:00:00")
                it "interprets '6' as 6:00 PM" do
                    Normalize.clockedOutAt "6" `shouldBe` Just (toTimeOfDay "18:00:00")
                it "interprets '7' as 7:00 PM" do
                    Normalize.clockedOutAt "7" `shouldBe` Just (toTimeOfDay "19:00:00")
                it "interprets '8' as 8:00 PM" do
                    Normalize.clockedOutAt "8" `shouldBe` Just (toTimeOfDay "20:00:00")
                it "interprets '9' as 9:00 PM" do
                    Normalize.clockedOutAt "9" `shouldBe` Just (toTimeOfDay "21:00:00")
                it "interprets '0' as nothing" do
                    Normalize.clockedOutAt "0" `shouldBe` Nothing

            context "two digits" do
                it "interprets '10' as 10:00 AM" do
                    Normalize.clockedOutAt "10" `shouldBe` Just (toTimeOfDay "10:00:00")
                it "interprets '11' as 11:00 AM" do
                    Normalize.clockedOutAt "11" `shouldBe` Just (toTimeOfDay "11:00:00")
                it "interprets '12' as 12:00 PM" do
                    Normalize.clockedOutAt "12" `shouldBe` Just (toTimeOfDay "12:00:00")
                it "interprets '13' as 1:00 PM" do
                    Normalize.clockedOutAt "13" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '14' as 2:00 PM" do
                    Normalize.clockedOutAt "14" `shouldBe` Just (toTimeOfDay "14:00:00")
                it "interprets '15' as 3:00 PM" do
                    Normalize.clockedOutAt "15" `shouldBe` Just (toTimeOfDay "15:00:00")
                it "interprets '16' as 4:00 PM" do
                    Normalize.clockedOutAt "16" `shouldBe` Just (toTimeOfDay "16:00:00")
                it "interprets '17' as 5:00 PM" do
                    Normalize.clockedOutAt "17" `shouldBe` Just (toTimeOfDay "17:00:00")
                it "interprets '18' as 6:00 PM" do
                    Normalize.clockedOutAt "18" `shouldBe` Just (toTimeOfDay "18:00:00")
                it "interprets '19' as 7:00 PM" do
                    Normalize.clockedOutAt "19" `shouldBe` Just (toTimeOfDay "19:00:00")
                it "interprets '20' as 8:00 PM" do
                    Normalize.clockedOutAt "20" `shouldBe` Just (toTimeOfDay "20:00:00")
                it "interprets '21' as 9:00 PM" do
                    Normalize.clockedOutAt "21" `shouldBe` Just (toTimeOfDay "21:00:00")
                it "interprets '22' as 10:00 PM" do
                    Normalize.clockedOutAt "22" `shouldBe` Just (toTimeOfDay "22:00:00")
                it "interprets '23' as 11:00 PM" do
                    Normalize.clockedOutAt "23" `shouldBe` Just (toTimeOfDay "23:00:00")
                it "interprets '24' as nothing" do
                    Normalize.clockedOutAt "24" `shouldBe` Nothing

            context "three digits" do
                it "interprets '130' as 1:30 PM" do
                    Normalize.clockedOutAt "130" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '230' as 2:30 PM" do
                    Normalize.clockedOutAt "230" `shouldBe` Just (toTimeOfDay "14:30:00")
                it "interprets '330' as 3:30 PM" do
                    Normalize.clockedOutAt "330" `shouldBe` Just (toTimeOfDay "15:30:00")
                it "interprets '430' as 4:30 PM" do
                    Normalize.clockedOutAt "430" `shouldBe` Just (toTimeOfDay "16:30:00")
                it "interprets '530' as 5:30 PM" do
                    Normalize.clockedOutAt "530" `shouldBe` Just (toTimeOfDay "17:30:00")
                it "interprets '630' as 6:30 AM" do
                    Normalize.clockedOutAt "630" `shouldBe` Just (toTimeOfDay "18:30:00")
                it "interprets '730' as 7:30 PM" do
                    Normalize.clockedOutAt "730" `shouldBe` Just (toTimeOfDay "19:30:00")
                it "interprets '830' as 8:30 PM" do
                    Normalize.clockedOutAt "830" `shouldBe` Just (toTimeOfDay "20:30:00")
                it "interprets '930' as 9:30 PM" do
                    Normalize.clockedOutAt "930" `shouldBe` Just (toTimeOfDay "21:30:00")
                it "interprets '145' as 1:45 PM" do
                    Normalize.clockedOutAt "145" `shouldBe` Just (toTimeOfDay "13:45:00")
                it "interprets '1:30' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '030' as nothing" do
                    Normalize.clockedOutAt "030" `shouldBe` Nothing

            context "four digits (military time)" do
                it "interprets '0130' as 1:30 AM" do
                    Normalize.clockedOutAt "0130" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '0230' as 2:30 AM" do
                    Normalize.clockedOutAt "0230" `shouldBe` Just (toTimeOfDay "02:30:00")
                it "interprets '0330' as 3:30 AM" do
                    Normalize.clockedOutAt "0330" `shouldBe` Just (toTimeOfDay "03:30:00")
                it "interprets '0430' as 4:30 AM" do
                    Normalize.clockedOutAt "0430" `shouldBe` Just (toTimeOfDay "04:30:00")
                it "interprets '0530' as 5:30 AM" do
                    Normalize.clockedOutAt "0530" `shouldBe` Just (toTimeOfDay "05:30:00")
                it "interprets '0630' as 6:30 AM" do
                    Normalize.clockedOutAt "0630" `shouldBe` Just (toTimeOfDay "06:30:00")
                it "interprets '0730' as 7:30 AM" do
                    Normalize.clockedOutAt "0730" `shouldBe` Just (toTimeOfDay "07:30:00")
                it "interprets '0830' as 8:30 AM" do
                    Normalize.clockedOutAt "0830" `shouldBe` Just (toTimeOfDay "08:30:00")
                it "interprets '0930' as 1:30 AM" do
                    Normalize.clockedOutAt "0930" `shouldBe` Just (toTimeOfDay "09:30:00")
                it "interprets '1030' as 10:30 AM" do
                    Normalize.clockedOutAt "1030" `shouldBe` Just (toTimeOfDay "10:30:00")
                it "interprets '1130' as 11:30 AM" do
                    Normalize.clockedOutAt "1130" `shouldBe` Just (toTimeOfDay "11:30:00")
                it "interprets '1230' as 12:30 PM" do
                    Normalize.clockedOutAt "1230" `shouldBe` Just (toTimeOfDay "12:30:00")
                it "interprets '1330' as 1:30 PM" do
                    Normalize.clockedOutAt "1330" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '1430' as 2:30 PM" do
                    Normalize.clockedOutAt "1430" `shouldBe` Just (toTimeOfDay "14:30:00")
                it "interprets '1530' as 3:30 PM" do
                    Normalize.clockedOutAt "1530" `shouldBe` Just (toTimeOfDay "15:30:00")
                it "interprets '1630' as 4:30 PM" do
                    Normalize.clockedOutAt "1630" `shouldBe` Just (toTimeOfDay "16:30:00")
                it "interprets '1730' as 5:30 PM" do
                    Normalize.clockedOutAt "1730" `shouldBe` Just (toTimeOfDay "17:30:00")
                it "interprets '1830' as 6:30 PM" do
                    Normalize.clockedOutAt "1830" `shouldBe` Just (toTimeOfDay "18:30:00")
                it "interprets '1930' as 7:30 PM" do
                    Normalize.clockedOutAt "1930" `shouldBe` Just (toTimeOfDay "19:30:00")
                it "interprets '2030' as 8:30 PM" do
                    Normalize.clockedOutAt "2030" `shouldBe` Just (toTimeOfDay "20:30:00")
                it "interprets '2130' as 21:30 PM" do
                    Normalize.clockedOutAt "2130" `shouldBe` Just (toTimeOfDay "21:30:00")
                it "interprets '2230' as 22:30 PM" do
                    Normalize.clockedOutAt "2230" `shouldBe` Just (toTimeOfDay "22:30:00")
                it "interprets '2330' as 23:30 PM" do
                    Normalize.clockedOutAt "2330" `shouldBe` Just (toTimeOfDay "23:30:00")
                it "interprets '01:30' as 1:30 AM" do
                    Normalize.clockedOutAt "01:30" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '2430' as nothing" do
                    Normalize.clockedOutAt "2430" `shouldBe` Nothing

            context "digits with am" do
                it "interprets '1a' as 1:00 AM" do
                    Normalize.clockedOutAt "1a" `shouldBe` Just (toTimeOfDay "01:00:00")
                it "interprets '2a' as 2:00 AM" do
                    Normalize.clockedOutAt "2a" `shouldBe` Just (toTimeOfDay "02:00:00")
                it "interprets '3a' as 3:00 AM" do
                    Normalize.clockedOutAt "3a" `shouldBe` Just (toTimeOfDay "03:00:00")
                it "interprets '4a' as 4:00 AM" do
                    Normalize.clockedOutAt "4a" `shouldBe` Just (toTimeOfDay "04:00:00")
                it "interprets '5a' as 5:00 AM" do
                    Normalize.clockedOutAt "5a" `shouldBe` Just (toTimeOfDay "05:00:00")
                it "interprets '6a' as 6:00 AM" do
                    Normalize.clockedOutAt "6a" `shouldBe` Just (toTimeOfDay "06:00:00")
                it "interprets '7a' as 7:00 AM" do
                    Normalize.clockedOutAt "7a" `shouldBe` Just (toTimeOfDay "07:00:00")
                it "interprets '8a' as 8:00 AM" do
                    Normalize.clockedOutAt "8a" `shouldBe` Just (toTimeOfDay "08:00:00")
                it "interprets '9a' as 9:00 AM" do
                    Normalize.clockedOutAt "9a" `shouldBe` Just (toTimeOfDay "09:00:00")
                it "interprets '10a' as 10:00 AM" do
                    Normalize.clockedOutAt "10a" `shouldBe` Just (toTimeOfDay "10:00:00")
                it "interprets 11a' as 11:00 AM" do
                    Normalize.clockedOutAt "11a" `shouldBe` Just (toTimeOfDay "11:00:00")
                it "interprets '12a' as 12:00 AM" do
                    Normalize.clockedOutAt "12a" `shouldBe` Just (toTimeOfDay "00:00:00")
                it "interprets '1am' as 1:00 AM" do
                    Normalize.clockedOutAt "1am" `shouldBe` Just (toTimeOfDay "01:00:00")
                it "interprets '1 am' as 1:00 AM" do
                    Normalize.clockedOutAt "1 am" `shouldBe` Just (toTimeOfDay "01:00:00")
                it "interprets '1A' as 1:00 AM" do
                    Normalize.clockedOutAt "1A" `shouldBe` Just (toTimeOfDay "01:00:00")
                it "interprets '1AM' as 1:00 AM" do
                    Normalize.clockedOutAt "1AM" `shouldBe` Just (toTimeOfDay "01:00:00")
                it "interprets '1 AM' as 1:00 AM" do
                    Normalize.clockedOutAt "1 AM" `shouldBe` Just (toTimeOfDay "01:00:00")

            context "digits with pm" do
                it "interprets '1p' as 1:00 PM" do
                    Normalize.clockedOutAt "1p" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '2p' as 2:00 PM" do
                    Normalize.clockedOutAt "2p" `shouldBe` Just (toTimeOfDay "14:00:00")
                it "interprets '3p' as 3:00 PM" do
                    Normalize.clockedOutAt "3p" `shouldBe` Just (toTimeOfDay "15:00:00")
                it "interprets '4p' as 4:00 PM" do
                    Normalize.clockedOutAt "4p" `shouldBe` Just (toTimeOfDay "16:00:00")
                it "interprets '5p' as 5:00 PM" do
                    Normalize.clockedOutAt "5p" `shouldBe` Just (toTimeOfDay "17:00:00")
                it "interprets '6p' as 6:00 PM" do
                    Normalize.clockedOutAt "6p" `shouldBe` Just (toTimeOfDay "18:00:00")
                it "interprets '7p' as 7:00 PM" do
                    Normalize.clockedOutAt "7p" `shouldBe` Just (toTimeOfDay "19:00:00")
                it "interprets '8p' as 8:00 PM" do
                    Normalize.clockedOutAt "8p" `shouldBe` Just (toTimeOfDay "20:00:00")
                it "interprets '9p' as 9:00 PM" do
                    Normalize.clockedOutAt "9p" `shouldBe` Just (toTimeOfDay "21:00:00")
                it "interprets '10p' as 10:00 PM" do
                    Normalize.clockedOutAt "10p" `shouldBe` Just (toTimeOfDay "22:00:00")
                it "interprets 11p' as 11:00 PM" do
                    Normalize.clockedOutAt "11p" `shouldBe` Just (toTimeOfDay "23:00:00")
                it "interprets '12p' as 12:00 PM" do
                    Normalize.clockedOutAt "12p" `shouldBe` Just (toTimeOfDay "12:00:00")
                it "interprets '1pm' as 1:00 PM" do
                    Normalize.clockedOutAt "1pm" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '1 pm' as 1:00 PM" do
                    Normalize.clockedOutAt "1 pm" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '1P' as 1:00 PM" do
                    Normalize.clockedOutAt "1P" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '1PM' as 1:00 PM" do
                    Normalize.clockedOutAt "1PM" `shouldBe` Just (toTimeOfDay "13:00:00")
                it "interprets '1 PM' as 1:00 PM" do
                    Normalize.clockedOutAt "1 PM" `shouldBe` Just (toTimeOfDay "13:00:00")

            context "multiple digits with am" do
                it "interprets '1:30a' as 1:30 AM" do
                    Normalize.clockedOutAt "1:30a" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '2:30a' as 2:30 AM" do
                    Normalize.clockedOutAt "2:30a" `shouldBe` Just (toTimeOfDay "02:30:00")
                it "interprets '3:30a' as 3:30 AM" do
                    Normalize.clockedOutAt "3:30a" `shouldBe` Just (toTimeOfDay "03:30:00")
                it "interprets '4:30a' as 4:30 AM" do
                    Normalize.clockedOutAt "4:30a" `shouldBe` Just (toTimeOfDay "04:30:00")
                it "interprets '5:30a' as 5:30 AM" do
                    Normalize.clockedOutAt "5:30a" `shouldBe` Just (toTimeOfDay "05:30:00")
                it "interprets '6:30a' as 6:30 AM" do
                    Normalize.clockedOutAt "6:30a" `shouldBe` Just (toTimeOfDay "06:30:00")
                it "interprets '7:30a' as 1:30 AM" do
                    Normalize.clockedOutAt "7:30a" `shouldBe` Just (toTimeOfDay "07:30:00")
                it "interprets '8:30a' as 8:30 AM" do
                    Normalize.clockedOutAt "8:30a" `shouldBe` Just (toTimeOfDay "08:30:00")
                it "interprets '9:30a' as 9:30 AM" do
                    Normalize.clockedOutAt "9:30a" `shouldBe` Just (toTimeOfDay "09:30:00")
                it "interprets '10:30a' as 10:30 AM" do
                    Normalize.clockedOutAt "10:30a" `shouldBe` Just (toTimeOfDay "10:30:00")
                it "interprets '11:30a' as 11:30 AM" do
                    Normalize.clockedOutAt "11:30a" `shouldBe` Just (toTimeOfDay "11:30:00")
                it "interprets '12:30a' as 12:30 AM" do
                    Normalize.clockedOutAt "12:30a" `shouldBe` Just (toTimeOfDay "00:30:00")
                it "interprets '1:30A' as 1:30 AM" do
                    Normalize.clockedOutAt "1:30A" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '1:30 a' as 1:30 AM" do
                    Normalize.clockedOutAt "1:30 a" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '1:30 A' as 1:30 AM" do
                    Normalize.clockedOutAt "1:30 A" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '1:30 am' as 1:30 AM" do
                    Normalize.clockedOutAt "1:30 am" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '1:30 AM' as 1:30 AM" do
                    Normalize.clockedOutAt "1:30 AM" `shouldBe` Just (toTimeOfDay "01:30:00")
                it "interprets '01:30 AM' as nothing" do
                    Normalize.clockedOutAt "01:30 AM" `shouldBe` Nothing
                it "interprets '13:30 AM' as nothing" do
                    Normalize.clockedOutAt "13:30 AM" `shouldBe` Nothing

            context "multiple digits with pm" do
                it "interprets '1:30p' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30p" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '2:30p' as 2:30 PM" do
                    Normalize.clockedOutAt "2:30p" `shouldBe` Just (toTimeOfDay "14:30:00")
                it "interprets '3:30p' as 3:30 PM" do
                    Normalize.clockedOutAt "3:30p" `shouldBe` Just (toTimeOfDay "15:30:00")
                it "interprets '4:30p' as 4:30 PM" do
                    Normalize.clockedOutAt "4:30p" `shouldBe` Just (toTimeOfDay "16:30:00")
                it "interprets '5:30p' as 5:30 PM" do
                    Normalize.clockedOutAt "5:30p" `shouldBe` Just (toTimeOfDay "17:30:00")
                it "interprets '6:30p' as 6:30 PM" do
                    Normalize.clockedOutAt "6:30p" `shouldBe` Just (toTimeOfDay "18:30:00")
                it "interprets '7:30p' as 1:30 PM" do
                    Normalize.clockedOutAt "7:30p" `shouldBe` Just (toTimeOfDay "19:30:00")
                it "interprets '8:30p' as 8:30 PM" do
                    Normalize.clockedOutAt "8:30p" `shouldBe` Just (toTimeOfDay "20:30:00")
                it "interprets '9:30p' as 9:30 PM" do
                    Normalize.clockedOutAt "9:30p" `shouldBe` Just (toTimeOfDay "21:30:00")
                it "interprets '10:30p' as 10:30 PM" do
                    Normalize.clockedOutAt "10:30p" `shouldBe` Just (toTimeOfDay "22:30:00")
                it "interprets '11:30p' as 11:30 PM" do
                    Normalize.clockedOutAt "11:30p" `shouldBe` Just (toTimeOfDay "23:30:00")
                it "interprets '12:30p' as 12:30 PM" do
                    Normalize.clockedOutAt "12:30p" `shouldBe` Just (toTimeOfDay "12:30:00")
                it "interprets '1:30p' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30p" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '1:30P' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30P" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '1:30 p' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30 p" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '1:30 P' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30 P" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '1:30 pm' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30 pm" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '1:30 PM' as 1:30 PM" do
                    Normalize.clockedOutAt "1:30 PM" `shouldBe` Just (toTimeOfDay "13:30:00")
                it "interprets '01:30 PM' as nothing" do
                    Normalize.clockedOutAt "01:30 PM" `shouldBe` Nothing
                it "interprets '13:30 PM' as nothing" do
                    Normalize.clockedOutAt "13:30 PM" `shouldBe` Nothing

    describe "hoursWorked" do
        context "decimal number" do
            it "interprets '8' as 8.0 hours" do
                Normalize.hoursWorked "8" `shouldBe` Just 8.0
            it "interprets '7' as 7.0 hours" do
                Normalize.hoursWorked "7" `shouldBe` Just 7.0
            it "interprets '8.0' as 8.0 hours" do
                Normalize.hoursWorked "8.0" `shouldBe` Just 8.0
            it "interprets '8.5' as 8.5 hours" do
                Normalize.hoursWorked "8.5" `shouldBe` Just 8.5
            it "interprets '8hr' as 8.0 hours" do
                Normalize.hoursWorked "8hr" `shouldBe` Just 8.0
            it "interprets '8hrs' as 8.0 hours" do
                Normalize.hoursWorked "8hrs" `shouldBe` Just 8.0
            it "interprets '8 hrs' as 8.0 hours" do
                Normalize.hoursWorked "8 hrs" `shouldBe` Just 8.0
            it "interprets '8.0 hrs' as 8.0 hours" do
                Normalize.hoursWorked "8.0 hrs" `shouldBe` Just 8.0
            it "interprets '8.0  hrs' as 8.0 hours" do
                Normalize.hoursWorked "8.0  hrs" `shouldBe` Just 8.0
            it "interprets '8.123 hrs' as 8.123 hours" do
                Normalize.hoursWorked "8.123 hrs" `shouldBe` Just 8.123
            it "interprets '8. hrs' as 8.0 hours" do
                Normalize.hoursWorked "8. hrs" `shouldBe` Just 8.0
            it "interprets '8.0 hours' as 8.0 hours" do
                Normalize.hoursWorked "8.0 hours" `shouldBe` Just 8.0
            it "interprets '8h' as 8.0 hours" do
                Normalize.hoursWorked "8h" `shouldBe` Just 8.0
            it "interprets '8 Hrs' as 8.0 hours" do
                Normalize.hoursWorked "8 Hrs" `shouldBe` Just 8.0
            it "interprets '8hours' as 8.0 hours" do
                Normalize.hoursWorked "8hours" `shouldBe` Just 8.0
            it "interprets '8x' as nothing" do
                Normalize.hoursWorked "8x" `shouldBe` Nothing

        context "fractional hour" do
            it "interprets '8 1/2' as 8.5 hours" do
                Normalize.hoursWorked "8 1/2" `shouldBe` Just 8.5
            it "interprets '8 1/2hr' as 8.5 hours" do
                Normalize.hoursWorked "8 1/2hr" `shouldBe` Just 8.5
            it "interprets '81/2hrs' as 8.0 hours" do
                Normalize.hoursWorked "81/2hrs" `shouldBe` Just 8.5
            it "interprets '8 1/2 hrs' as 8.0 hours" do
                Normalize.hoursWorked "8 1/2 hrs" `shouldBe` Just 8.5
            it "interprets '8.0 1/2 hrs' as nothing" do
                Normalize.hoursWorked "8.0 1/2 hrs" `shouldBe` Nothing
            it "interprets '8   1/2  hrs' as 8.0 hours" do
                Normalize.hoursWorked "8   1/2  hrs" `shouldBe` Just 8.5
            it "interprets '8.123 1/2 hrs' as nothing" do
                Normalize.hoursWorked "8.123 1/2 hrs" `shouldBe` Nothing
            it "interprets '8. 1/2 hrs' as nothing" do
                Normalize.hoursWorked "8. 1/2 hrs" `shouldBe` Nothing
            it "interprets '8.0 1/2 hours' as 8.0 1/2 hours" do
                Normalize.hoursWorked "8.0 1/2 hours" `shouldBe` Nothing
            it "interprets '81/2h' as 8.5 hours" do
                Normalize.hoursWorked "81/2h" `shouldBe` Just 8.5
            it "interprets '8 1/2h' as 8.5 hours" do
                Normalize.hoursWorked "8 1/2h" `shouldBe` Just 8.5
            it "interprets '8 1/2 Hrs' as 8.5 hours" do
                Normalize.hoursWorked "8 1/2 Hrs" `shouldBe` Just 8.5
            it "interprets '81/2hours' as 8.0 hours" do
                Normalize.hoursWorked "81/2hours" `shouldBe` Just 8.5
            it "interprets '8 1/4' as nothing" do
                Normalize.hoursWorked "8 1/4" `shouldBe` Nothing
            it "interprets '8 1/2a' as nothing" do
                Normalize.hoursWorked "8 1/2a" `shouldBe` Nothing

    describe "jobName" do
        it "returns nothing if there is no previous or current job name" do
            Normalize.jobName Nothing Nothing `shouldBe` Nothing
        it "returns the previous job name if there is no current job name" do
            Normalize.jobName (Just "123 Something Rd.") Nothing `shouldBe` Just "123 Something Rd."
        it "returns the current job name if there is no previous job name" do
            Normalize.jobName Nothing (Just "123 Something Rd.") `shouldBe` Just "123 Something Rd."
        it "returns the previous job name if the current job name is a substring of the previous job name" do
            Normalize.jobName (Just "123 Something Rd.") (Just "Something") `shouldBe` Just "123 Something Rd."
        it "returns the current job name if the current job name is not a substring of the previous job name" do
            Normalize.jobName (Just "123 Something Rd.") (Just "Else") `shouldBe` Just "Else"
        it "ignores case when comparing strings" do
            Normalize.jobName (Just "123 Something Rd.") (Just "something") `shouldBe` Just "123 Something Rd."
