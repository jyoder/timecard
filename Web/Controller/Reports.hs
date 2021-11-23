module Web.Controller.Reports where

import qualified Application.Reports.AutomationQuery as Reports.AutomationQuery
import Web.Controller.Prelude
import Web.View.Reports.Index

instance Controller ReportsController where
    beforeAction = ensureIsUser

    action ReportsAction = do
        today <- utctDay <$> getCurrentTime
        let startDate = addDays (-28) today

        dailyReportRows <-
            Reports.AutomationQuery.fetch
                Reports.AutomationQuery.ByDay
                startDate
                today

        render IndexView {..}
