module Web.Controller.TimecardReviews where

import Web.Controller.Prelude
import Web.View.TimecardReviews.Show

instance Controller TimecardReviewsController where
    action ShowTimecardReviewAction = do
        render ShowView
