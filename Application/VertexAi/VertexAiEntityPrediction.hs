module Application.VertexAi.VertexAiEntityPrediction (
    validate,
) where

import Application.Service.Validation (nonBlank)
import Generated.Types
import IHP.ControllerPrelude

validate :: VertexAiEntityPrediction -> VertexAiEntityPrediction
validate vertexAiEntityPrediction =
    let segmentStartOffset = get #segmentStartOffset vertexAiEntityPrediction
     in vertexAiEntityPrediction
            |> validateField #vertexAiId nonBlank
            |> validateField #displayName nonBlank
            |> validateField
                #segmentStartOffset
                ( validateAny [isInList [0], isGreaterThan 0]
                    |> withCustomErrorMessage "This field must be greater than or equal to 0"
                )
            |> validateField
                #segmentEndOffset
                ( validateAny [isInList [segmentStartOffset], isGreaterThan segmentStartOffset]
                    |> withCustomErrorMessage "This field must be greater than or equal to segmentStartOffset"
                )
            |> validateField
                #confidence
                ( validateAny [isInList [0.0], isGreaterThan 0.0]
                    |> withCustomErrorMessage "This field must be greater than or equal to 0.0"
                )
            |> validateField
                #confidence
                ( validateAny [isInList [1.0], isLessThan 1.0]
                    |> withCustomErrorMessage "This field must be less than or equal to 1.0"
                )