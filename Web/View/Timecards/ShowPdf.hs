module Web.View.Timecards.ShowPdf where

import qualified Application.Timecard.View as V
import Web.View.Prelude hiding (Page)
import Web.View.Service.Time (formatDay)
import qualified Prelude as P

data ShowPdfView = ShowPdfView
    { selectedPerson :: !Person
    , timecard :: !V.Timecard
    , signing :: !(Maybe Signing)
    }

data Page = Page
    { timecardTable :: !TimecardTable
    , signatureBlock :: !SignatureBlock
    }
    deriving (Eq, Show)

data TimecardTable = TimecardTable
    { weekOf :: !Text
    , firstName :: !Text
    , lastName :: !Text
    , jobRows :: ![JobRow]
    , totalHoursRow :: !TotalHoursRow
    }
    deriving (Eq, Show)

data JobRow = JobRow
    { dayOfWeek' :: !Text
    , date :: !Text
    , jobName :: !Text
    , hoursWorked :: !Text
    , workDone :: !Text
    }
    deriving (Eq, Show)

newtype TotalHoursRow = TotalHoursRow
    { totalHours :: Text
    }
    deriving (Eq, Show)

data SignatureBlock
    = SignedBlock
        { name :: !Text
        , signedAt :: !Text
        , ipAddress :: !Text
        }
    | NotSignedBlock
    deriving (Eq, Show)

instance View ShowPdfView where
    beforeRender view = setLayout P.id

    html view = renderPage $ buildPage view

buildPage :: ShowPdfView -> Page
buildPage ShowPdfView {..} =
    Page
        { timecardTable = buildTimecardTable selectedPerson timecard
        , signatureBlock = buildSignatureBlock signing
        }

buildTimecardTable :: Person -> V.Timecard -> TimecardTable
buildTimecardTable selectedPerson timecard =
    TimecardTable
        { weekOf = formatDay $ get #weekOf timecard
        , firstName = get #firstName selectedPerson
        , lastName = get #lastName selectedPerson
        , jobRows = buildJobRow selectedPerson <$> get #entries timecard
        , totalHoursRow = buildTotalHoursRow $ get #entries timecard
        }

buildJobRow :: Person -> V.TimecardEntry -> JobRow
buildJobRow selectedPerson timecardEntry =
    JobRow
        { dayOfWeek' = show $ dayOfWeek $ get #date timecardEntry
        , date = formatDay $ get #date timecardEntry
        , jobName = get #jobName timecardEntry
        , hoursWorked = show $ get #hoursWorked timecardEntry
        , workDone = get #workDone timecardEntry
        }

buildTotalHoursRow :: [V.TimecardEntry] -> TotalHoursRow
buildTotalHoursRow timecardEntries =
    TotalHoursRow
        { totalHours = show $ sum $ get #hoursWorked <$> timecardEntries
        }

buildSignatureBlock :: Maybe Signing -> SignatureBlock
buildSignatureBlock (Just signing) =
    SignedBlock
        { name = get #name signing
        , signedAt = show $ get #signedAt signing
        , ipAddress = get #ipAddress signing
        }
buildSignatureBlock Nothing =
    NotSignedBlock

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
        <div class="content container-fluid">
            <div class="col-12">
                {renderTimecardTable timecardTable}
            </div>
            <div class="col-12">
                {renderSignatureCard signatureBlock}
            </div>
        </div>

        {styles}
    |]

renderTimecardTable :: TimecardTable -> Html
renderTimecardTable TimecardTable {..} =
    [hsx|
        <div class="card mb-5">
            <h5 class="card-header">
                Week Of {weekOf}
            </h5>
            
            <div class="card-body">
                <h5 class="card-title">
                    {lastName}, {firstName}
                </h5>

                <table class="table">
                    <thead>
                        <tr>
                            <th scope="col">Day</th>
                            <th scope="col">Date</th>
                            <th scope="col">Job</th>
                            <th scope="col">Work Done</th>
                            <th scope="col">Hours</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach jobRows renderJobRow}
                        {renderTotalHoursRow totalHoursRow}
                    </tbody>
                </table>
            </div>
        </div>
    |]

renderJobRow :: JobRow -> Html
renderJobRow JobRow {..} =
    [hsx|
        <tr>
            <th scope="row">{dayOfWeek'}</th>
            <td>{date}</td>
            <td>{jobName}</td>
            <td class="work-done">{workDone}</td>
            <td>{hoursWorked}</td>
        </tr>
    |]

renderTotalHoursRow :: TotalHoursRow -> Html
renderTotalHoursRow TotalHoursRow {..} =
    [hsx|
        <tr class="table-secondary">
            <th scope="row">Total Hours</th>
            <td></td>
            <td></td>
            <td></td>
            <td>{totalHours}</td>
        </tr>
    |]

renderSignatureCard :: SignatureBlock -> Html
renderSignatureCard signatureBlock =
    [hsx|
        <div class="card mb-5">
            <h5 class="card-header">
                Electronic Signature
            </h5>
            <div class="card-body">
                {renderSignatureBlock signatureBlock}
            </div>  
        </div>
    |]

renderSignatureBlock :: SignatureBlock -> Html
renderSignatureBlock SignedBlock {..} =
    [hsx|
        <ul class="list-group">
            <li class="list-group-item"><strong>Signed by:&nbsp</strong>{name}</li>
            <li class="list-group-item"><strong>Signed at:&nbsp</strong>{signedAt}</li>
            <li class="list-group-item"><strong>IP address:&nbsp</strong>{ipAddress}</li>
        </ul>
    |]
renderSignatureBlock NotSignedBlock =
    [hsx|
        <p>Timecard not yet signed.</p>
    |]

styles :: Html
styles =
    [hsx|
        <style>
            .work-done {
                width: 300px;
            }
        </style>
    |]
