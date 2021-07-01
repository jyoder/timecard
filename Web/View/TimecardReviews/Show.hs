module Web.View.TimecardReviews.Show where

import qualified Application.Timecard.Query as Q
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.View.Prelude
import Web.View.Service.Time (formatDay)

newtype ShowView = ShowView
    { reviewStatus :: ReviewStatus
    }

data ReviewStatus
    = ReviewNotFound
    | ReviewExpired
    | ReviewFound
        { person :: !Person
        , timecard :: !Q.Timecard
        , accessToken :: !AccessToken
        , signing :: !Signing
        }

instance View ShowView where
    html ShowView {..} =
        [hsx|
            {renderNavigation}
            {renderReview reviewStatus}
        |]

renderNavigation :: Html
renderNavigation =
    [hsx|
        <nav class="navbar navbar-expand navbar-light bg-light">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1" href="#">Constructable</span>
                <div class="collapse navbar-collapse" id="navbarSupportedContent">
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

renderReview :: ReviewStatus -> Html
renderReview reviewStatus = do
    case reviewStatus of
        ReviewNotFound -> renderNotFound
        ReviewExpired -> renderExpired
        ReviewFound {..} -> renderFound person timecard accessToken signing

renderNotFound :: Html
renderNotFound =
    [hsx|
        <div class="alert alert-dark d-flex justify-content-center" role="alert">
            Sorry, this link doesn't look valid. Just text us and we'll send you a new one!
        </div>
    |]

renderExpired :: Html
renderExpired =
    [hsx|
        <div class="alert alert-dark" role="alert">
            Sorry, this link has expired. Just text us and we'll send you a new one!
        </div>
    |]

renderFound :: Person -> Q.Timecard -> AccessToken -> Signing -> Html
renderFound person timecard accessToken signing =
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
                Week of {formatDay $ get #weekOf timecard}
            </h3>
            <div class="col-sm-12 col-lg-6">
                {renderTimecardEntries timecard}
                {renderTotalHours timecard}
                {renderSignatureBlock accessToken signing}
            </div>
        </div>
    |]
  where
    lastName = get #lastName person
    firstName = get #firstName person

renderTimecardEntries :: Q.Timecard -> Html
renderTimecardEntries Q.Timecard {..} =
    [hsx|
        <ul class="list-group">
            {forEach entries renderTimecardEntry}
        </ul>
    |]

renderTimecardEntry :: Q.TimecardEntry -> Html
renderTimecardEntry timecardEntry =
    [hsx|
        <div class="card mb-4">
            <h5 class="card-header">
                {dayOfWeek date} - {formatDay date}
            </h5>

            <div class="card-body">
                <h5 class="card-title">{hoursWorked} hours - {jobName}</h5>
                <p class="card-text">{workDone}</p>
            </div>
        </div>
    |]
  where
    date = get #date timecardEntry
    jobName = get #jobName timecardEntry
    hoursWorked = get #hoursWorked timecardEntry
    workDone = get #workDone timecardEntry

renderTotalHours :: Q.Timecard -> Html
renderTotalHours timecard =
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
  where
    totalHours = sum hours
    hours = get #hoursWorked <$> entries
    entries = get #entries timecard

renderSignatureBlock :: AccessToken -> Signing -> Html
renderSignatureBlock accessToken signing =
    [hsx|
        <div class="card mb-4">
            <div class="card-body">
                <h5 class="card-title">Signature</h5>
                {if isNew signing
                    then renderSignatureForm accessToken signing
                    else renderSignatureComplete signing}
            </div>
        </div>
    |]

renderSignatureForm :: AccessToken -> Signing -> Html
renderSignatureForm accessToken signing =
    formFor'
        signing
        (pathTo CreateSigningAction)
        [hsx|
            <p class="mb-4">I've reviewed the timecard above and it's all correct:</p>

            {(textField #name) 
                { 
                  fieldLabel = "Your full name"
                , fieldClass = "col-sm-12 col-lg-6"
                , autofocus = True
                , fieldInput = (\fieldInput -> H.input ! A.autocomplete "off") 
                }}
            
            <input 
                type="hidden"
                name="accessTokenValue"
                id="accessTokenValue"
                value={get #value accessToken}
            />

            {submitButton { label = "Sign"}}
        |]

renderSignatureComplete :: Signing -> Html
renderSignatureComplete signing =
    [hsx|
        <p class="alert alert-success" role="alert">
            This timecard has been successfully signed! You are all done and can close this window.
        </p>

        <p>
            <strong>Signed by:</strong> {get #name signing}
        </p>
        <p>
            <strong>Signed at:</strong> {get #signedAt signing}
        </p>
        <p>
            <strong>IP address:</strong> {get #ipAddress signing}
        </p>
    |]
  where
    signedAt = get #signedAt signing