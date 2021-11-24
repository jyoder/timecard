module Tests.Web.View.Reports.IndexSpec where

import qualified Application.Reports.AutomationQuery as Reports.AutomationQuery
import IHP.Prelude
import Test.Hspec
import Tests.Support
import qualified Web.View.Reports.Index as Reports.Index

spec :: Spec
spec = do
    describe "buildPage" do
        it "builds an automation report page" do
            Reports.Index.buildPage
                Reports.Index.IndexView
                    { dailyReportRows =
                        [ Reports.AutomationQuery.Row
                            { date = toDay "2021-11-22"
                            , personId = "10000000-0000-0000-0000-000000000000"
                            , personFirstName = "Bob"
                            , personLastName = "Bobbers"
                            , automationStatus = Reports.AutomationQuery.FullyAutomated
                            }
                        , Reports.AutomationQuery.Row
                            { date = toDay "2021-11-21"
                            , personId = "10000000-0000-0000-0000-000000000000"
                            , personFirstName = "Bob"
                            , personLastName = "Bobbers"
                            , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                            }
                        , Reports.AutomationQuery.Row
                            { date = toDay "2021-11-22"
                            , personId = "20000000-0000-0000-0000-000000000000"
                            , personFirstName = "Rob"
                            , personLastName = "Robbers"
                            , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                            }
                        , Reports.AutomationQuery.Row
                            { date = toDay "2021-11-21"
                            , personId = "20000000-0000-0000-0000-000000000000"
                            , personFirstName = "Rob"
                            , personLastName = "Robbers"
                            , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                            }
                        ]
                    }
                `shouldBe` Reports.Index.Page
                    { dailyReportTable =
                        Reports.Index.Table
                            { rows =
                                [ Reports.Index.Row
                                    { name = "Bobbers, Bob"
                                    , automationRate = "50%"
                                    , cells =
                                        [ Reports.Index.Cell {date = "11/22", classes = "fully-automated"}
                                        , Reports.Index.Cell {date = "", classes = "not-fully-automated"}
                                        ]
                                    }
                                , Reports.Index.Row
                                    { name = "Robbers, Rob"
                                    , automationRate = "0%"
                                    , cells =
                                        [ Reports.Index.Cell {date = "11/22", classes = "not-fully-automated"}
                                        , Reports.Index.Cell {date = "", classes = "not-fully-automated"}
                                        ]
                                    }
                                ]
                            }
                    }

    describe "buildTable" do
        it "builds an automation report table" do
            Reports.Index.buildTable
                [ Reports.AutomationQuery.Row
                    { date = toDay "2021-11-22"
                    , personId = "10000000-0000-0000-0000-000000000000"
                    , personFirstName = "Bob"
                    , personLastName = "Bobbers"
                    , automationStatus = Reports.AutomationQuery.FullyAutomated
                    }
                , Reports.AutomationQuery.Row
                    { date = toDay "2021-11-21"
                    , personId = "10000000-0000-0000-0000-000000000000"
                    , personFirstName = "Bob"
                    , personLastName = "Bobbers"
                    , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                    }
                , Reports.AutomationQuery.Row
                    { date = toDay "2021-11-22"
                    , personId = "20000000-0000-0000-0000-000000000000"
                    , personFirstName = "Rob"
                    , personLastName = "Robbers"
                    , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                    }
                , Reports.AutomationQuery.Row
                    { date = toDay "2021-11-21"
                    , personId = "20000000-0000-0000-0000-000000000000"
                    , personFirstName = "Rob"
                    , personLastName = "Robbers"
                    , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                    }
                ]
                `shouldBe` Reports.Index.Table
                    { rows =
                        [ Reports.Index.Row
                            { name = "Bobbers, Bob"
                            , automationRate = "50%"
                            , cells =
                                [ Reports.Index.Cell {date = "11/22", classes = "fully-automated"}
                                , Reports.Index.Cell {date = "", classes = "not-fully-automated"}
                                ]
                            }
                        , Reports.Index.Row
                            { name = "Robbers, Rob"
                            , automationRate = "0%"
                            , cells =
                                [ Reports.Index.Cell {date = "11/22", classes = "not-fully-automated"}
                                , Reports.Index.Cell {date = "", classes = "not-fully-automated"}
                                ]
                            }
                        ]
                    }

    describe "buildRow" do
        it "builds a row in the automation rate table with the correct automation rate and cells" do
            Reports.Index.buildRow
                [ Reports.AutomationQuery.Row
                    { date = toDay "2021-11-22"
                    , personId = "10000000-0000-0000-0000-000000000000"
                    , personFirstName = "Bob"
                    , personLastName = "Bobbers"
                    , automationStatus = Reports.AutomationQuery.FullyAutomated
                    }
                , Reports.AutomationQuery.Row
                    { date = toDay "2021-11-21"
                    , personId = "10000000-0000-0000-0000-000000000000"
                    , personFirstName = "Bob"
                    , personLastName = "Bobbers"
                    , automationStatus = Reports.AutomationQuery.NotFullyAutomated
                    }
                ]
                `shouldBe` Reports.Index.Row
                    { name = "Bobbers, Bob"
                    , automationRate = "50%"
                    , cells =
                        [ Reports.Index.Cell {date = "11/22", classes = "fully-automated"}
                        , Reports.Index.Cell {date = "", classes = "not-fully-automated"}
                        ]
                    }

    describe "formatName" do
        it "returns a name with the format last, first" do
            Reports.Index.formatName
                [ Reports.AutomationQuery.Row
                    { date = toDay "2021-11-22"
                    , personId = "10000000-0000-0000-0000-000000000000"
                    , personFirstName = "Bob"
                    , personLastName = "Bobbers"
                    , automationStatus = Reports.AutomationQuery.FullyAutomated
                    }
                ]
                `shouldBe` "Bobbers, Bob"

        it "returns a default string if there are no rows" do
            Reports.Index.formatName [] `shouldBe` "Unknown Person"

    describe "buildCell" do
        it "returns a table cell based on the given properties" do
            Reports.Index.buildCell
                Reports.AutomationQuery.Row
                    { date = toDay "2021-11-22"
                    , personId = "10000000-0000-0000-0000-000000000000"
                    , personFirstName = "Bob"
                    , personLastName = "Bobbers"
                    , automationStatus = Reports.AutomationQuery.FullyAutomated
                    }
                `shouldBe` Reports.Index.Cell {date = "11/22", classes = "fully-automated"}

    describe "formatDateIfMonday" do
        it "returns a date with the format mm/dd if the given date is a Monday" do
            Reports.Index.formatDateIfMonday (toDay "2021-11-22") `shouldBe` "11/22"
        it "returns an empty string if the date is not a Monday" do
            Reports.Index.formatDateIfMonday (toDay "2021-11-23") `shouldBe` ""

    describe "automationStatusClass" do
        it "returns fully-automated when the status is fully automated" do
            Reports.Index.automationStatusClass Reports.AutomationQuery.FullyAutomated `shouldBe` "fully-automated"
        it "returns not-fully-automated when the status is not fully automated" do
            Reports.Index.automationStatusClass Reports.AutomationQuery.NotFullyAutomated `shouldBe` "not-fully-automated"
        it "returns no-activity when the status indicates no activity" do
            Reports.Index.automationStatusClass Reports.AutomationQuery.NoActivity `shouldBe` "no-activity"