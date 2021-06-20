module Web.View.TimecardReviews.Show where

import Web.View.Prelude

data ShowView = ShowView

instance View ShowView where
    html ShowView =
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
