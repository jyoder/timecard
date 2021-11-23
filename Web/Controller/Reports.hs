module Web.Controller.Reports where

import qualified Application.Reports.AutomationQuery as Reports.AutomationQuery
import Web.Controller.Prelude
import Web.View.Reports.Index

instance Controller ReportsController where
    beforeAction = ensureIsUser

    action ReportsAction = do
        now <- getCurrentTime
        dailyReportRows <-
            Reports.AutomationQuery.fetch
                Reports.AutomationQuery.ByDay
                (utctDay now)

        render IndexView {..}
