module Web.View.Layout (defaultLayout, Html) where

import Generated.Types
import IHP.Controller.RequestContext
import IHP.Environment
import IHP.ViewPrelude
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes
import Web.Types

defaultLayout :: Html -> Html
defaultLayout inner =
    H.docTypeHtml ! A.lang "en" $
        [hsx|
            <head>
                {metaTags}

                {stylesheets}
                {scripts}

                {fullStoryIfProduction}

                <title>Constructable</title>
            </head>
            <body>
                <div class="content container-fluid p-0 m-0">
                    {renderFlashMessages}
                    {inner}
                </div>
            </body>
        |]

stylesheets :: Html
stylesheets =
    [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]

scripts :: Html
scripts =
    [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/vendor/turbolinks.js"></script>
        <script src="/vendor/turbolinksInstantClick.js"></script>
        <script src="/vendor/turbolinksMorphdom.js"></script>
        <script src="/helpers.js"></script>
        <script src="/ihp-auto-refresh.js"></script>
        <script src="app.js"></script>
    |]

metaTags :: Html
metaTags =
    [hsx|
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        <meta property="og:title" content="App"/>
        <meta property="og:type" content="website"/>
        <meta property="og:url" content="TODO"/>
        <meta property="og:description" content="TODO"/>
        {autoRefreshMeta}
    |]

fullStoryIfProduction :: Html
fullStoryIfProduction =
    if fromConfig environment == Production
        then fullStoryBase
        else [hsx||]

fullStory :: Html
fullStory =
    [hsx|
        {fullStoryBase}
        {fullStoryUser maybeUser}
    |]
  where
    maybeUser = maybeFromFrozenContext @User

fullStoryBase :: Html
fullStoryBase =
    [hsx|
        <script>
            window['_fs_debug'] = false;
            window['_fs_host'] = 'fullstory.com';
            window['_fs_script'] = 'edge.fullstory.com/s/fs.js';
            window['_fs_org'] = '11NNCD';
            window['_fs_namespace'] = 'FS';
            (function(m,n,e,t,l,o,g,y){
                if (e in m) {if(m.console && m.console.log) { m.console.log('FullStory namespace conflict. Please set window["_fs_namespace"].');} return;}
                g=m[e]=function(a,b,s){g.q?g.q.push([a,b,s]):g._api(a,b,s);};g.q=[];
                o=n.createElement(t);o.async=1;o.crossOrigin='anonymous';o.src='https://'+_fs_script;
                y=n.getElementsByTagName(t)[0];y.parentNode.insertBefore(o,y);
                g.identify=function(i,v,s){g(l,{uid:i},s);if(v)g(l,v,s)};g.setUserVars=function(v,s){g(l,v,s)};g.event=function(i,v,s){g('event',{n:i,p:v},s)};
                g.anonymize=function(){g.identify(!!0)};
                g.shutdown=function(){g("rec",!1)};g.restart=function(){g("rec",!0)};
                g.log = function(a,b){g("log",[a,b])};
                g.consent=function(a){g("consent",!arguments.length||a)};
                g.identifyAccount=function(i,v){o='account';v=v||{};v.acctId=i;g(o,v)};
                g.clearUserCookie=function(){};
                g.setVars=function(n, p){g('setVars',[n,p]);};
                g._w={};y='XMLHttpRequest';g._w[y]=m[y];y='fetch';g._w[y]=m[y];
                if(m[y])m[y]=function(){return g._w[y].apply(this,arguments)};
                g._v="1.3.0";
            })(window,document,window['_fs_namespace'],'script','user');
        </script>
    |]

fullStoryUser :: Maybe User -> Html
fullStoryUser (Just user) =
    [hsx|
        <script data-user-id={userId} data-user-email={userEmail}>
            FS.identify(document.currentScript.dataset.userId, {
                email: document.currentScript.dataset.userEmail
            });
        </script>
    |]
  where
    userId = show $ get #id user
    userEmail = show $ get #email user
fullStoryUser Nothing = [hsx||]
