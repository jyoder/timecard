module Web.View.Service.Style (
    removeScrollbars,
) where

import Web.View.Prelude

removeScrollbars :: Html
removeScrollbars =
    [hsx|
        <style>
            /* Remove the scrollbar from Chrome, Safari, Edge and IE. */
            ::-webkit-scrollbar {
                width: 0px;
                background: transparent;
            }

            * {
                -ms-overflow-style: none !important;
            }
        </style>
|]
