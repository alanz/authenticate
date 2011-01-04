{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Web.Authenticate.Twitter where

import Network.HTTP.Enumerator
import Data.List (intercalate)
import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Web.Authenticate.Internal (qsEncode)
import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Maybe
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.PercentEncoding
import Control.Monad
import Control.Monad.Trans


{-
data Twitter = Twitter
    { twitterClientId :: String
    , twitterClientSecret :: String
    , twitterRedirectUri :: String
    }
    deriving (Show, Eq, Read, Ord, Data, Typeable)

newtype AccessToken = AccessToken { unAccessToken :: String }
    deriving (Show, Eq, Read, Ord, Data, Typeable)

getForwardUrl :: Twitter -> [String] -> String
getForwardUrl twt perms = concat
    [ "https://twitter.com/oauth/authorize?client_id="
    , qsEncode $ twitterClientId twt
    , "&redirect_uri="
    , qsEncode $ twitterRedirectUri twt
    , if null perms
        then ""
        else "&scope=" ++ qsEncode (intercalate "," perms)
    ]

accessTokenUrl :: Twitter -> String -> String
accessTokenUrl twt code = concat
     --  http://api.twitter.com/oauth/request_token
    [ "https://twitter.com/oauth/access_token?client_id="
    , qsEncode $ twitterClientId twt
    , "&redirect_uri="
    , qsEncode $ twitterRedirectUri twt
    , "&client_secret="
    , qsEncode $ twitterClientSecret twt
    , "&code="
    , qsEncode code
    ]

getAccessToken :: Twitter -> String -> IO AccessToken
getAccessToken twt code = do
    let url = accessTokenUrl twt code
    b <- simpleHttp url
    let (front, back) = splitAt 13 $ L8.unpack b
    case front of
        "access_token=" -> return $ AccessToken back
        _ -> error $ "Invalid twitter response: " ++ back

graphUrl :: AccessToken -> String -> String
graphUrl (AccessToken s) func = concat
    [ "https://graph.facebook.com/"
    , func
    , "?access_token="
    , s
    ]

getGraphData :: AccessToken -> String -> IO StringObject
getGraphData at func = do
    let url = graphUrl at func
    b <- simpleHttp url
    decode $ S.concat $ L.toChunks b

-}
-- ---------------------------------------------------------------------


reqUrl   = fromJust $ parseURL "http://twitter.com/oauth/request_token"
accUrl   = fromJust $ parseURL "http://twitter.com/oauth/access_token"
srvUrl   = fromJust $ parseURL "http://api.twitter.com/1/statuses/home_timeline.xml"
--authUrl  = ("https://twitter.com/oauth/authorize?oauth_token="++) . findWithDefault ("oauth_token","") . oauthParams
authUrl  = ("https://twitter.com/oauth/authenticate?oauth_token="++) . findWithDefault ("oauth_token","") . oauthParams
app      = Application "OziWIpIJJV9p8MfV6IaZkg" "Lyp4EspBofyUnXgm1vzEMN9MDkxLswcCaqYd4ud9Q" (URL "http://localhost:3000/")
--app      = Application "OziWIpIJJV9p8MfV6IaZkg" "Lyp4EspBofyUnXgm1vzEMN9MDkxLswcCaqYd4ud9Q" OOB
token    = fromApplication app

response ::  (MonadIO m) => m Network.OAuth.Http.Response.Response
response = runOAuthM token $ do { ignite app
                                ; signRq2 HMACSHA1 Nothing reqUrl >>= oauthRequest CurlClient
                                ; cliAskAuthorization authUrl
                                ; signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient
                                ; signRq2 HMACSHA1 Nothing srvUrl >>= serviceRequest CurlClient
                                }

-- ---------------------------------------------------------------------

-- Implement http://dev.twitter.com/pages/sign_in_with_twitter


-- Used by Twitter to replace section 6.1 of OAUTH 1.0
-- http://dev.twitter.com/doc/get/oauth/authenticate
--authenticateUrl = fromJust $ parseURL "http://api.twitter.com/oauth/authenticate "

-- Example from http://twimply.com/twitter_login.php
--  http://twitter.com/oauth/authorize?oauth_token=lvAQA96sov5GWtgq85ttBArETCLPn71t6cJTNs0hzQ

-- EOF
