module Slack.Plugin
( HandlerFn
, HelpStr
, NameStr
, Plugin (..)
, Reply  (..)
, TokenStr
, apply
, isAuthorized
, newPlugin
, noReply
, selectFrom
, viaHaskbot
) where

import Data.List (find)
import Data.Text (Text)

import Slack.SlashCom (SlashCom, command, token)
import Slack.Types
import Slack.Incoming (Incoming (..), enqueue)

type NameStr   = Text
type HelpStr   = Text
type HandlerFn = SlashCom -> IO Reply
type TokenStr  = Text

data Reply = ViaHaskbot Incoming
           | NoReply

data Plugin = Plugin { plCommand  :: Command
                     , plHelpText :: Text
                     , plHandler  :: HandlerFn
                     , plToken    :: Token
                     }

apply :: Plugin -> SlashCom -> IO ()
apply plugin slashCom = do
  reply <- plHandler plugin slashCom
  case reply of
    ViaHaskbot incoming -> enqueue incoming
    _                   -> return ()

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

newPlugin :: Text -> Text -> HandlerFn -> Text -> Plugin
newPlugin com help handler token =
  Plugin (setCommand com) help handler (setToken token)

noReply :: (Monad m) => m Reply
noReply = return NoReply

selectFrom :: [Plugin] -> Command -> Maybe Plugin
selectFrom list com = find (\p -> plCommand p == com) list

viaHaskbot :: (Monad m) => Channel -> Text -> m Reply
viaHaskbot chan = return . ViaHaskbot . Incoming chan
