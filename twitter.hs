{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Yesod.Form
import Web.Authenticate.Twitter
import Data.Object
import Data.Maybe (fromMaybe)
import Network.HTTP.Enumerator


data TWT = TWT Twitter
twt :: TWT
twt = TWT $ Twitter "OziWIpIJJV9p8MfV6IaZkg" "Lyp4EspBofyUnXgm1vzEMN9MDkxLswcCaqYd4ud9Q"
                   "http://localhost:3000/twitter/"
mkYesod "TWT" [$parseRoutes|
--/ HomeR GET
/ RootR GET
/forward ForwardR GET
/twitter TwitterR GET
|]

instance Yesod TWT where approot _ = "http://localhost:3000"

-- getHomeR = hamletToRepHtml [$hamlet|
getHomeR = defaultLayout [$hamlet|
%h1 Hello World!
|]

{-
%p Here are some of my favorite links:
%ul
    %li
        %a!href="http://docs.yesodweb.com/" Yesod Web Framework Docs
    %li
        %a!href="http://www.haskell.org/" Haskell Homepage
%p Thanks for visiting!
-}

getRootR = do
    TWT t <- getYesod
    -- getForwardUrl openid $ render CompleteR
    -- redirectString RedirectTemporary $ getForwardUrl f ["email"]
    --let url = "http://blah"
    let url = getForwardUrl t ["email"]
    defaultLayout [$hamlet|
%h1 Haskell Twitter Sign in Test Page
%p 
  %a!href=$url$ Sign in with Twitter 
%p 
  %a!href="http://twitter.com/oauth/authorize?oauth_token=" Sign in with Twitter 2
%p          
  %a!href="http://docs.yesodweb.com/" Yesod Web Framework Docs
|]

getForwardR = do
    openid <- runFormGet' $ stringInput "openid_identifier"
    render <- getUrlRender
    -- url <- liftIO $ getForwardUrl openid $ render CompleteR
    -- redirectString RedirectTemporary url
    return ()

getOtherR = do
    TWT t <- getYesod
    redirectString RedirectTemporary $ getForwardUrl t ["email"]
    return ()

getTwitterR = do
    TWT f <- getYesod
    code <- runFormGet' $ stringInput "code"
    at <- liftIO $ getAccessToken f code
    mreq <- runFormGet' $ maybeStringInput "req"
    let req = fromMaybe "me" mreq
    so <- liftIO $ getGraphData at req
    let so' = objToHamlet so
    hamletToRepHtml [$hamlet|
%form
    %input!type=hidden!name=code!value=$string.code$
    Request: $
    %input!type=text!name=req!value=$string.req$
    \ $
    %input!type=submit
%hr
^so'^
|]

-- main = withHttpEnumerator $ basicHandler 3000 twt
main = withHttpEnumerator $ warpDebug 3000 twt

objToHamlet :: StringObject -> Hamlet url
objToHamlet (Scalar s) = [$hamlet|$string.s$|]
objToHamlet (Sequence list) = [$hamlet|
%ul
    $forall list o
        %li ^objToHamlet.o^
|]
objToHamlet (Mapping pairs) = [$hamlet|
%dl
    $forall pairs pair
        %dt $string.fst.pair$
        %dd ^objToHamlet.snd.pair^
|]
