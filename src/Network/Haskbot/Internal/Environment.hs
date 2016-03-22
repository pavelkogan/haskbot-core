{-# LANGUAGE RecordWildCards #-}

module Network.Haskbot.Internal.Environment
( Environment (..)
, bootstrap
, HaskbotM
, EnvironT
) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Connection as N
import Network.Haskbot.Config (Config)
import qualified Network.HTTP.Conduit as N
import Network.HTTP.Types (Status)

data Environment = Environment { incQueue :: TVar [BL.ByteString]
                               , netConn  :: N.Manager
                               , config   :: Config
                               }

type EnvironT m = ReaderT Environment m
type HaskbotM   = EnvironT (ExceptT Status IO)

-- internal functions

bootstrap :: Config -> IO Environment
bootstrap config = do
  incQueue    <- newTVarIO []
  netConn     <- defNetConn >>= N.newManager
  return Environment{..}

-- private functions

defNetConn :: IO N.ManagerSettings
defNetConn = return $ N.mkManagerSettings tlsInfo Nothing
  where tlsInfo = N.TLSSettingsSimple False False False
