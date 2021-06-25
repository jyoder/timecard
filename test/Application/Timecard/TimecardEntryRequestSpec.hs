module Application.Timecard.TimecardEntryRequestSpec where

import Application.Timecard.TimecardEntryRequest
import IHP.Prelude
import IHP.Test.Mocking
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = do
    describe "nextRequestTimea" $ do
        describe "Monday" $ do
            it "returns request time for Monday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-14 15:30:00 PDT"
            it "returns request time for Tuesday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-14 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-15 15:30:00 PDT"

        describe "Tuesday" $ do
            it "returns request time for Tuesday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-15 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-15 15:30:00 PDT"
            it "returns request time for Wednesday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-15 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-16 15:30:00 PDT"

        describe "Wednesday" $ do
            it "returns request time for Wednesday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-16 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-16 15:30:00 PDT"
            it "returns request time for Thursday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-16 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-17 15:30:00 PDT"

        describe "Thursday" $ do
            it "returns request time for Thursday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-17 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-17 15:30:00 PDT"
            it "returns request time for Friday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-17 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-18 15:30:00 PDT"

        describe "Friday" $ do
            it "returns request time for Friday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-18 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-18 15:30:00 PDT"
            it "returns request time for Monday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-18 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-21 15:30:00 PDT"

        describe "Saturday" $ do
            it "returns request time for Monday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-19 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-21 15:30:00 PDT"
            it "returns request time for Monday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-19 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-21 15:30:00 PDT"

        describe "Sunday" $ do
            it "returns request time for Monday if current time is before deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-20 15:29:00 PDT")
                    `shouldBe` toUtc "2021-06-21 15:30:00 PDT"
            it "returns request time for Monday if current time is after deadline" $ do
                nextRequestTime pdt requestTimeOfDay (toUtc "2021-06-20 15:30:00 PDT")
                    `shouldBe` toUtc "2021-06-21 15:30:00 PDT"

        describe "different timezone" $ do
            it "returns the proper request time for the specified timezone" $ do
                nextRequestTime est requestTimeOfDay (toUtc "2021-06-14 15:29:00 EST")
                    `shouldBe` toUtc "2021-06-14 15:30:00 EST"

        describe "alternate request time of day" $ do
            it "returns a request time that corresponds to the given request time of day" $ do
                nextRequestTime pdt (TimeOfDay 5 40 20) (toUtc "2021-06-14 05:39:00 PDT")
                    `shouldBe` toUtc "2021-06-14 05:40:20 PDT"

toUtc :: String -> UTCTime
toUtc time = zonedTimeToUTC (read time :: ZonedTime)

requestTimeOfDay :: TimeOfDay
requestTimeOfDay = TimeOfDay 15 30 00

pdt :: TimeZone
pdt = read "PDT"

est :: TimeZone
est = read "EST"