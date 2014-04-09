{-# LANGUAGE OverloadedStrings #-} 
module HSlackBot where

import Control.Applicative
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Monoid (mconcat)
import Web.Scotty

type Port = Int

{-
POST will be sent to all URLs defined like so:

token=2OMF6jGOFHgQm6ANRZFswxHi
team_id=T0001
channel_id=C2147483705
channel_name=test
timestamp=1355517523.000005
user_id=U2147483697
user_name=Steve
text=googlebot: What is the air-speed velocity of an unladen swallow?
trigger_word=googlebot:
-}
data SlackRequest = SlackRequest
    { token         :: String
    , team_id       :: String
    , channel_id    :: String
    , channel_name  :: String
    , timestamp     :: String
    , user_id       :: String
    , user_name     :: String
    , msg_text      :: String
    , trigger_word  :: String
    }

{- Response:
    { "text": "African or European?" }
-}
data SlackResponse = SlackResponse String
instance ToJSON SlackResponse where
  toJSON (SlackResponse resp) = 
    object [ "text" .= resp ]

start :: Port -> IO ()
start port = scotty port $ do
    get "/" $ do
        html "hello world"
    post "/jira" $ do
        jiraPath    <- param "jiraPath"
        req         <- SlackRequest
                        <$> param "token"
                        <*> param "team_id"
                        <*> param "channel_id"
                        <*> param "channel_name"
                        <*> param "timestamp"
                        <*> param "user_id"
                        <*> param "user_name"
                        <*> param "text"
                        <*> param "trigger_word"

        json $ SlackResponse $ mconcat 
                [ trigger_word req
                , "\n"
                , jiraPath
                , "\n"
                , msg_text req
                ]

jiraResponder :: SlackRequest -> Maybe SlackResponse
jiraResponder = undefined
