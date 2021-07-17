module Application.Action.ActionRunTimeSpec where

import qualified Application.Action.ActionRunTime as ActionRunTime
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec

spec :: Spec
spec = do
    describe "validate" do
        it "currently does nothing" do
            let actionRunTime = newRecord @ActionRunTime
            actionRunTime |> ActionRunTime.validate `shouldBe` actionRunTime