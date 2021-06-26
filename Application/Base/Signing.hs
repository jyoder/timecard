module Application.Base.Signing (
    validate,
) where

import Application.Service.Validation
import Generated.Types
import IHP.ControllerPrelude hiding (create)

validate :: Signing -> Signing
validate signing =
    signing
        |> validateField #name nonEmpty
        |> validateField #ipAddress nonEmpty
