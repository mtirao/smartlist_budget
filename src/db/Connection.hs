{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Connection (getConnection) where

import Control.Monad.IO.Class
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hardcoded

getConnection :: IO (Either ConnectionError Connection)
getConnection = acquire $ settings Hardcoded.host  Hardcoded.portNumber Hardcoded.user Hardcoded.password Hardcoded.database
