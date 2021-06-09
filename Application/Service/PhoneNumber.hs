module Application.Service.PhoneNumber (validate) where

import Generated.Types
import IHP.Prelude
import IHP.ValidationSupport.ValidateField

validate :: PhoneNumber -> PhoneNumber
validate phoneNumber =
    phoneNumber
        |> validateField #number isPhoneNumber
