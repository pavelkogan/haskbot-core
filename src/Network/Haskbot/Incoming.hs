-- | This provides a simple representation of the request data for a Slack
--   /incoming/ integration- the means via which Haskbot replies to Slack.
--   Currently only simple text replies are supported, but this will be expanded
--   to support fully-slack-formatted messages in the future.
module Network.Haskbot.Incoming
( Incoming (..)
) where

import Data.Text (Text)
import Network.Haskbot.Types (Channel)

data Incoming = Incoming { incChan ::                !Channel -- ^ the channel to send the reply
                         , incText :: {-# UNPACK #-} !Text    -- ^ the text of the reply
                         } deriving (Eq, Show)