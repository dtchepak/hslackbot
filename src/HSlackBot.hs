{-# LANGUAGE OverloadedStrings #-} 
module HSlackBot where

import Control.Applicative
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Network.HTTP.Types.Status
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Web.Scotty
import Web.Scotty.TLS (scottyTLS)

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
    { token       :: String
    , teamId      :: String
    , channelId   :: String
    , channelName :: String
    , timestamp   :: String
    , userId      :: String
    , userName    :: String
    , msgText     :: String
    , triggerWord :: String
    }

{- Response:
    { "text": "African or European?" }
-}
data SlackResponse = SlackResponse String
instance ToJSON SlackResponse where
  toJSON (SlackResponse resp) = 
    object [ "text" .= resp ]

type JiraPath = String
type JiraProject = String
type JiraIssue = String

start :: Port -> IO ()
start port = scotty port app

startSSL :: FilePath -> FilePath -> Port -> IO ()
startSSL key cert port = scottyTLS port key cert app

app :: ScottyM()
app = do
    get "/" $ html "hello world"
    post "/jira" $ do
        jiraPath    <- param "jiraPath"
        jiraProjs   <- param "jiraProjects"
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
        maybe (status noContent204) json $
            proj `parse` jiraProjs >>= \p -> jiraResponder jiraPath p req

parse :: P.Parser a -> String -> Maybe a
parse p = either (const Nothing) Just . P.parse p []

proj :: TokenParsing m => m [JiraProject]
proj = commaSep (some upper)

issueParser :: (Monad m, TokenParsing m) => [JiraProject] -> m JiraIssue
issueParser projs = do
    ref <- choice (string <$> projs)
    char '-'
    num <- natural
    return $ ref ++ '-' : show num

issuesParser :: (Monad m, TokenParsing m) => [JiraProject] -> m [JiraIssue]
issuesParser projs =
    let refs = Just <$> issueParser projs
    in catMaybes <$> many (try refs <|> (anyChar *> return Nothing))

jiraResponder ::
    JiraPath
    -> [JiraProject]
    -> SlackRequest
    -> Maybe SlackResponse
jiraResponder path projs req =
    let toMsg = intercalate "\n"
        issuesToPath = fmap (path++)
        respP = (SlackResponse . toMsg . issuesToPath) <$> issuesParser projs
    in respP `parse` msgText req
