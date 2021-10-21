module Tests.Application.Twilio.ViewSpec where

import qualified Application.Twilio.Query as Query
import qualified Application.Twilio.View as View
import Data.List.NonEmpty (NonEmpty (..))
import Generated.Types
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "buildMessages" do
        it "returns an empty list when no rows are given" do
            View.buildMessages [] `shouldBe` []

        it "returns multiple distinct messages for rows with distinct ids" do
            View.buildMessages
                [ Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Just "Hello"
                    , entityType = Just Query.JobName
                    , entityStart = Just 0
                    , entityEnd = Just 5
                    , entityConfidence = Just 0.4
                    }
                , Query.Row
                    { id = "20000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Just "World"
                    , entityType = Nothing
                    , entityStart = Nothing
                    , entityEnd = Nothing
                    , entityConfidence = Nothing
                    }
                ]
                `shouldBe` [ View.Message
                                { id = "10000000-0000-0000-0000-000000000000"
                                , fromPhoneNumber = "+1111111111"
                                , fromFirstName = "Bob"
                                , fromLastName = "Bobbers"
                                , toPhoneNumber = "+2222222222"
                                , toFirstName = "Bill"
                                , toLastName = "Billers"
                                , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                                , status = Query.Delivered
                                , body = "Hello"
                                , entities =
                                    [ View.Entity
                                        { entityType = Query.JobName
                                        , rawText = "Hello"
                                        , confidence = 0.4
                                        }
                                    ]
                                }
                           , View.Message
                                { id = "20000000-0000-0000-0000-000000000000"
                                , fromPhoneNumber = "+1111111111"
                                , fromFirstName = "Bob"
                                , fromLastName = "Bobbers"
                                , toPhoneNumber = "+2222222222"
                                , toFirstName = "Bill"
                                , toLastName = "Billers"
                                , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                                , status = Query.Delivered
                                , body = "World"
                                , entities =
                                    [ View.Entity
                                        { entityType = Query.Unrecognized
                                        , rawText = "World"
                                        , confidence = 1.0
                                        }
                                    ]
                                }
                           ]

    describe "buildMessage" do
        it "converts multple rows into a message" do
            View.buildMessage
                ( Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Just "345 Gortock Rd. Hammered boards 7:30-4:30"
                    , entityType = Just Query.JobName
                    , entityStart = Just 0
                    , entityEnd = Just 15
                    , entityConfidence = Just 0.9
                    }
                    :| [ Query.Row
                            { id = "10000000-0000-0000-0000-000000000000"
                            , fromPhoneNumber = "+1111111111"
                            , fromFirstName = "Bob"
                            , fromLastName = "Bobbers"
                            , toPhoneNumber = "+2222222222"
                            , toFirstName = "Bill"
                            , toLastName = "Billers"
                            , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                            , status = Query.Delivered
                            , body = Nothing
                            , entityType = Just Query.ClockedInAt
                            , entityStart = Just 32
                            , entityEnd = Just 36
                            , entityConfidence = Just 0.8
                            }
                       ]
                )
                `shouldBe` View.Message
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = "345 Gortock Rd. Hammered boards 7:30-4:30"
                    , entities =
                        [ View.Entity
                            { entityType = Query.JobName
                            , rawText = "345 Gortock Rd."
                            , confidence = 0.9
                            }
                        , View.Entity
                            { entityType = Query.Unrecognized
                            , rawText = " Hammered boards "
                            , confidence = 1.0
                            }
                        , View.Entity
                            { entityType = Query.ClockedInAt
                            , rawText = "7:30"
                            , confidence = 0.8
                            }
                        , View.Entity
                            { entityType = Query.Unrecognized
                            , rawText = "-4:30"
                            , confidence = 1.0
                            }
                        ]
                    }

        it "requires the first row to contain the body" do
            View.buildMessage
                ( Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Nothing
                    , entityType = Just Query.JobName
                    , entityStart = Just 0
                    , entityEnd = Just 15
                    , entityConfidence = Just 0.9
                    }
                    :| [ Query.Row
                            { id = "10000000-0000-0000-0000-000000000000"
                            , fromPhoneNumber = "+1111111111"
                            , fromFirstName = "Bob"
                            , fromLastName = "Bobbers"
                            , toPhoneNumber = "+2222222222"
                            , toFirstName = "Bill"
                            , toLastName = "Billers"
                            , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                            , status = Query.Delivered
                            , body = Just "345 Gortock Rd. Hammered boards 7:30-4:30"
                            , entityType = Just Query.ClockedInAt
                            , entityStart = Just 32
                            , entityEnd = Just 36
                            , entityConfidence = Just 0.8
                            }
                       ]
                )
                `shouldBe` View.Message
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = "<<missing body>>"
                    , entities =
                        [ View.Entity
                            { entityType = Query.JobName
                            , rawText = "<<missing body>"
                            , confidence = 0.9
                            }
                        , View.Entity
                            { entityType = Query.Unrecognized
                            , rawText = ">"
                            , confidence = 1.0
                            }
                        , View.Entity
                            { entityType = Query.ClockedInAt
                            , rawText = ""
                            , confidence = 0.8
                            }
                        , View.Entity
                            { entityType = Query.Unrecognized
                            , rawText = ""
                            , confidence = 1.0
                            }
                        ]
                    }

    describe "buildEntities" do
        it "returns an entity that spans the entire body if no entity information is present" do
            View.buildEntities
                "345 Gortock Rd. Hammered boards 7:30-4:30"
                [ Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Nothing
                    , entityType = Nothing
                    , entityStart = Nothing
                    , entityEnd = Nothing
                    , entityConfidence = Nothing
                    }
                ]
                `shouldBe` [ View.Entity
                                { entityType = Query.Unrecognized
                                , rawText = "345 Gortock Rd. Hammered boards 7:30-4:30"
                                , confidence = 1.0
                                }
                           ]

        it "returns an entity that fills in spaces with unrecognized entities" do
            View.buildEntities
                "345 Gortock Rd. Hammered boards 7:30-4:30 8.5hrs"
                [ Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Nothing
                    , entityType = Just Query.ClockedInAt
                    , entityStart = Just 32
                    , entityEnd = Just 36
                    , entityConfidence = Just 0.7
                    }
                , Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Nothing
                    , entityType = Just Query.HoursWorked
                    , entityStart = Just 42
                    , entityEnd = Just 48
                    , entityConfidence = Just 0.8
                    }
                ]
                `shouldBe` [ View.Entity
                                { entityType = Query.Unrecognized
                                , rawText = "345 Gortock Rd. Hammered boards "
                                , confidence = 1.0
                                }
                           , View.Entity
                                { entityType = Query.ClockedInAt
                                , rawText = "7:30"
                                , confidence = 0.7
                                }
                           , View.Entity
                                { entityType = Query.Unrecognized
                                , rawText = "-4:30 "
                                , confidence = 1.0
                                }
                           , View.Entity
                                { entityType = Query.HoursWorked
                                , rawText = "8.5hrs"
                                , confidence = 0.8
                                }
                           ]

    describe "buildEntity" do
        it "returns an entity with the proper substring sliced from the body text" do
            View.buildEntity
                "hello world"
                View.EntityTuple
                    { entityType = Query.Unrecognized
                    , start = 6
                    , end = 9
                    , confidence = 1.0
                    }
                `shouldBe` View.Entity
                    { entityType = Query.Unrecognized
                    , rawText = "wor"
                    , confidence = 1.0
                    }

    describe "entityTuple" do
        it "translates a row to an entity tuple when all entity fields are present" do
            View.entityTuple
                Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Nothing
                    , entityType = Just Query.ClockedInAt
                    , entityStart = Just 32
                    , entityEnd = Just 36
                    , entityConfidence = Just 0.7
                    }
                `shouldBe` Just
                    ( View.EntityTuple
                        { entityType = Query.ClockedInAt
                        , start = 32
                        , end = 36
                        , confidence = 0.7
                        }
                    )
        it "returns nothing when any of the entity fields are missing" do
            View.entityTuple
                Query.Row
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromPhoneNumber = "+1111111111"
                    , fromFirstName = "Bob"
                    , fromLastName = "Bobbers"
                    , toPhoneNumber = "+2222222222"
                    , toFirstName = "Bill"
                    , toLastName = "Billers"
                    , createdAt = toUtc "2021-08-31 15:30:00 PDT"
                    , status = Query.Delivered
                    , body = Nothing
                    , entityType = Just Query.ClockedInAt
                    , entityStart = Nothing
                    , entityEnd = Just 36
                    , entityConfidence = Just 0.7
                    }
                `shouldBe` Nothing

    describe "fillSpaces" do
        it "fills spaces at the beginning, middle, and end of a set of tuples" do
            View.fillSpaces
                10
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 1
                    , end = 3
                    , confidence = 1.0
                    }
                , View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 5
                    , end = 7
                    , confidence = 1.0
                    }
                , View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 8
                    , end = 9
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 0
                                , end = 1
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 1
                                , end = 3
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 3
                                , end = 5
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 5
                                , end = 7
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 7
                                , end = 8
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 8
                                , end = 9
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 9
                                , end = 10
                                , confidence = 1.0
                                }
                           ]

    describe "fillSpacesBetween" do
        it "inserts a tuple in the empty space between each pair of tuples" do
            View.fillSpacesBetween
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 1
                    , end = 3
                    , confidence = 1.0
                    }
                , View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 5
                    , end = 7
                    , confidence = 1.0
                    }
                , View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 8
                    , end = 9
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 1
                                , end = 3
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 3
                                , end = 5
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 5
                                , end = 7
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 7
                                , end = 8
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 8
                                , end = 9
                                , confidence = 1.0
                                }
                           ]

        it "does not insert a tuple if there is no empty space between two tuples" do
            View.fillSpacesBetween
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 1
                    , end = 3
                    , confidence = 1.0
                    }
                , View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 3
                    , end = 5
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 1
                                , end = 3
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 3
                                , end = 5
                                , confidence = 1.0
                                }
                           ]

        it "returns an empty list if there are no tuples" do
            View.fillSpacesBetween [] `shouldBe` []

        it "returns the same list if there is only one tuple" do
            View.fillSpacesBetween
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 1
                    , end = 3
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 1
                                , end = 3
                                , confidence = 1.0
                                }
                           ]

    describe "fillSpaceBefore" do
        it "inserts a tuple that spans the whole range when the given list is empty" do
            View.fillSpaceBefore 10 []
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 0
                                , end = 10
                                , confidence = 1.0
                                }
                           ]
        it "does not insert a tuple when the first element starts at 0" do
            View.fillSpaceBefore
                10
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 0
                    , end = 5
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 0
                                , end = 5
                                , confidence = 1.0
                                }
                           ]
        it "inserts a tuple that fills the initial space in the range" do
            View.fillSpaceBefore
                10
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 5
                    , end = 8
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 0
                                , end = 5
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 5
                                , end = 8
                                , confidence = 1.0
                                }
                           ]

    describe "fillSpaceAfter" do
        it "inserts a tuple that spans the whole range when the given list is empty" do
            View.fillSpaceAfter 10 []
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 0
                                , end = 10
                                , confidence = 1.0
                                }
                           ]
        it "does not insert a tuple when the last element extends to the end of the range" do
            View.fillSpaceAfter
                10
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 5
                    , end = 10
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 5
                                , end = 10
                                , confidence = 1.0
                                }
                           ]
        it "inserts a tuple that fills the remaining space in the range" do
            View.fillSpaceAfter
                10
                [ View.EntityTuple
                    { entityType = Query.WorkDone
                    , start = 5
                    , end = 8
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ View.EntityTuple
                                { entityType = Query.WorkDone
                                , start = 5
                                , end = 8
                                , confidence = 1.0
                                }
                           , View.EntityTuple
                                { entityType = Query.Unrecognized
                                , start = 8
                                , end = 10
                                , confidence = 1.0
                                }
                           ]

    describe "unrecognizedTuple" do
        it "returns an unrecognized entity tuple with the given start, end, and 100% confidence" do
            View.unrecognizedTuple 3 10
                `shouldBe` View.EntityTuple
                    { entityType = Query.Unrecognized
                    , start = 3
                    , end = 10
                    , confidence = 1.0
                    }

    describe "slice" do
        it "returns the slice of text specified by the offsets, inclusive" do
            View.slice 0 3 "123456" `shouldBe` "123"
        it "returns the entire text if the end offset exceeds the length of the text" do
            View.slice 0 100 "123456" `shouldBe` "123456"
        it "treats negative starting offset as though it were zero" do
            View.slice (-10) 100 "123456" `shouldBe` "123456"
        it "returns an empty string if the end is less than the start" do
            View.slice 4 (-10) "123456" `shouldBe` ""
        it "returns an empty string if the start is equal to the end" do
            View.slice 4 4 "123456" `shouldBe` ""
