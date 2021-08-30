module Web.View.TimecardReviews.Show where

import qualified Application.Timecard.View as V
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.View.Prelude
import Web.View.Service.Time (formatDay, formatTimeOfDay)

newtype ShowView = ShowView
    { review :: Review
    }

data Review
    = ReviewNotFound
    | ReviewExpired
    | ReviewFound
        { person :: !Person
        , timecard :: !V.Timecard
        , accessToken :: !AccessToken
        , signing :: !Signing
        }

data ReviewStatus
    = ReviewNotFoundStatus
    | ReviewExpiredStatus
    | ReviewFoundStatus
        { firstName :: !Text
        , lastName :: !Text
        , weekOf :: !Text
        , timecardEntryCards :: ![TimecardEntryCard]
        , totalHoursCard :: !TotalHoursCard
        , signatureBlock :: !SignatureBlock
        , fullStoryScript :: !FullStoryScript
        }
    deriving (Eq, Show)

data TimecardEntryCard = TimecardEntryCard
    { day :: !Text
    , date :: !Text
    , hoursWorked :: !Text
    , jobName :: !Text
    , clockDetails :: !ClockDetails
    , workDone :: !Text
    }
    deriving (Eq, Show)

newtype TotalHoursCard = TotalHoursCard
    { totalHours :: Text
    }
    deriving (Eq, Show)

data ClockDetails
    = ClockDetails
        { clockedInAt :: !Text
        , clockedOutAt :: !Text
        , lunchDuration :: !Text
        }
    | NoClockDetails
    deriving (Eq, Show)

data SignatureBlock
    = NotSigned
        { signatureForm :: !SignatureForm
        }
    | Signed
        { completedSignature :: !CompletedSignature
        }
    deriving (Eq, Show)

data SignatureForm = SignatureForm
    { signing :: !Signing
    , signAction :: !TimecardReviewsController
    , accessToken :: !Text
    }
    deriving (Eq, Show)

data CompletedSignature = CompletedSignature
    { signedBy :: !Text
    , signedAt :: !Text
    , ipAddress :: !Text
    }
    deriving (Show, Eq)

data FullStoryScript = FullStoryScript
    { personId :: !Text
    , displayName :: !Text
    }
    deriving (Show, Eq)

instance View ShowView where
    html ShowView {..} = buildReviewStatus review |> renderReviewStatus

buildReviewStatus :: Review -> ReviewStatus
buildReviewStatus ReviewNotFound = ReviewNotFoundStatus
buildReviewStatus ReviewExpired = ReviewExpiredStatus
buildReviewStatus ReviewFound {..} =
    ReviewFoundStatus
        { firstName = get #firstName person
        , lastName = get #lastName person
        , weekOf = formatDay $ get #weekOf timecard
        , timecardEntryCards = buildTimecardEntryCards timecard
        , totalHoursCard = buildTotalHoursCard timecard
        , signatureBlock = buildSignatureBlock accessToken signing
        , fullStoryScript = buildFullStoryScript person
        }

buildTimecardEntryCards :: V.Timecard -> [TimecardEntryCard]
buildTimecardEntryCards V.Timecard {..} = buildTimecardEntryCard <$> entries

buildTimecardEntryCard :: V.TimecardEntry -> TimecardEntryCard
buildTimecardEntryCard timecardEntry =
    let V.TimecardEntry {..} = timecardEntry
     in TimecardEntryCard
            { day = show $ dayOfWeek date
            , date = formatDay date
            , hoursWorked = show hoursWorked
            , jobName = jobName
            , clockDetails = buildClockDetails timecardEntry
            , workDone = workDone
            }

buildClockDetails :: V.TimecardEntry -> ClockDetails
buildClockDetails V.TimecardEntry {..} =
    case (clockedInAt, clockedOutAt, lunchDuration) of
        (Just clockedInAt, Just clockedOutAt, Just lunchDuration) ->
            ClockDetails
                { clockedInAt = formatTimeOfDay clockedInAt
                , clockedOutAt = formatTimeOfDay clockedOutAt
                , lunchDuration = show lunchDuration
                }
        _ -> NoClockDetails

buildTotalHoursCard :: V.Timecard -> TotalHoursCard
buildTotalHoursCard V.Timecard {..} =
    TotalHoursCard
        { totalHours = show $ sum $ get #hoursWorked <$> entries
        }

buildSignatureBlock :: AccessToken -> Signing -> SignatureBlock
buildSignatureBlock accessToken signing =
    if isNew signing
        then NotSigned $ buildSignatureForm accessToken signing
        else Signed $ buildCompletedSignature signing

buildSignatureForm :: AccessToken -> Signing -> SignatureForm
buildSignatureForm accessToken signing =
    SignatureForm
        { signing = signing
        , signAction = CreateSigningAction
        , accessToken = get #value accessToken
        }

buildCompletedSignature :: Signing -> CompletedSignature
buildCompletedSignature signing =
    CompletedSignature
        { signedAt = show $ get #signedAt signing
        , signedBy = get #name signing
        , ipAddress = get #ipAddress signing
        }

buildFullStoryScript :: Person -> FullStoryScript
buildFullStoryScript person =
    FullStoryScript
        { personId = show $ get #id person
        , displayName = get #firstName person <> " " <> get #lastName person
        }

renderNavigation :: Html
renderNavigation =
    [hsx|
        <nav class="navbar navbar-expand navbar-light bg-light">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1" href="#">Constructable</span>
                <div class="collapse navbar-collapse">
                    <ul class="navbar-nav mb-0">
                        <li class="nav-item">
                            <span class="nav-link active" aria-current="true">
                                Review Timecard
                            </span>
                        </li>
                    </ul>
                </div>
            </div>
        </nav>
    |]

renderReviewStatus :: ReviewStatus -> Html
renderReviewStatus ReviewNotFoundStatus =
    [hsx|
        <div class="alert alert-dark d-flex justify-content-center" role="alert">
            Sorry, this link doesn't look valid. Just text us and we'll send you a new one!
        </div>
    |]
renderReviewStatus ReviewExpiredStatus =
    [hsx|
        <div class="alert alert-dark d-flex justify-content-center" role="alert">
            Sorry, this link has expired. Just text us and we'll send you a new one!
        </div>
    |]
renderReviewStatus ReviewFoundStatus {..} =
    [hsx|
        <div>
            <h1 class="mb-4">Review and sign</h1>

            <div class="mb-5">
                Please review and electronically sign your timecard below. Send us a text if anything doesn't look right. Thanks!
            </div>
        
            <h3>
                {firstName} {lastName}
            </h3>
            <h3 class="mb-5">
                Week of {weekOf}
            </h3>
            <div class="col-sm-12 col-lg-6">
                {renderTimecardEntryCards timecardEntryCards}
                {renderTotalHoursCard totalHoursCard}
                {renderSignatureBlock signatureBlock}
            </div>
        </div>

        {renderFullStoryScript fullStoryScript}
    |]

renderTimecardEntryCards :: [TimecardEntryCard] -> Html
renderTimecardEntryCards timecardEntryCards =
    [hsx|
        <ul class="list-group">
            {forEach timecardEntryCards renderTimecardEntryCard}
        </ul>
    |]

renderTimecardEntryCard :: TimecardEntryCard -> Html
renderTimecardEntryCard TimecardEntryCard {..} =
    [hsx|
        <div class="card mb-4">
            <h5 class="card-header">
                {day} - {date}
            </h5>

            <div class="card-body">
                <h5 class="card-title">{hoursWorked} hours - {jobName}</h5>
                <div class="card-text">
                    {renderClockDetails clockDetails}
                    <p>{nl2br workDone}</p>
                </div>
            </div>
        </div>
    |]

renderTotalHoursCard :: TotalHoursCard -> Html
renderTotalHoursCard TotalHoursCard {..} =
    [hsx|
        <div class="card mb-4">
            <h5 class="card-header">
                Total Hours Worked
            </h5>

            <div class="card-body">
                <h5>{totalHours} hours</h5>
            </div>
        </div>
    |]

renderClockDetails :: ClockDetails -> Html
renderClockDetails ClockDetails {..} =
    [hsx| <p>{clockedInAt}-{clockedOutAt} ({lunchDuration} min lunch)</p> |]
renderClockDetails NoClockDetails =
    [hsx||]

renderSignatureBlock :: SignatureBlock -> Html
renderSignatureBlock signatureBlock =
    [hsx|
        <div class="card mb-4">
            <div class="card-body">
                <h5 class="card-title">Signature</h5>
                    {renderSignatureBlock' signatureBlock}
            </div>
        </div>
    |]

renderSignatureBlock' :: SignatureBlock -> Html
renderSignatureBlock' NotSigned {..} = renderSignatureForm' signatureForm
renderSignatureBlock' Signed {..} = renderCompletedSignature completedSignature

renderSignatureForm' :: SignatureForm -> Html
renderSignatureForm' SignatureForm {..} =
    formFor'
        signing
        (pathTo signAction)
        [hsx|
            <p class="mb-4">I've reviewed the timecard above and it's all correct:</p>

            {(textField #name) 
                { 
                  fieldLabel = "Your full name"
                , fieldClass = "col-sm-12 col-lg-6"
                , autofocus = True
                , fieldInput = (\fieldInput -> H.input ! A.autocomplete "off") 
                }}
            
            <input type="hidden" name="accessTokenValue" value={accessToken}/>

            {submitButton { label = "Sign"}}
        |]

renderCompletedSignature :: CompletedSignature -> Html
renderCompletedSignature CompletedSignature {..} =
    [hsx|
        <p class="alert alert-success" role="alert">
            This timecard has been successfully signed! You are all done and can close this window.
        </p>

        <p><strong>Signed by:</strong> {signedBy}</p>
        <p><strong>Signed at:</strong> {signedAt}</p>
        <p><strong>IP address:</strong> {ipAddress}</p>
    |]

renderFullStoryScript :: FullStoryScript -> Html
renderFullStoryScript FullStoryScript {..} =
    [hsx|
        <script data-person-id={personId} data-display-name={displayName}>
            FS.identify(document.currentScript.dataset.personId, {
                displayName: document.currentScript.dataset.displayName
            });
        </script>
    |]
