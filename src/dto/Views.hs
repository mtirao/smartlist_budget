{-# LANGUAGE DeriveGeneric #-}

module Views where

import GHC.Generics()
import Web.Scotty as WS
import Data.Monoid()
import Data.Text()
import Data.Aeson
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class()
import Web.Scotty.Internal.Types()


jsonResponse :: ToJSON a => a -> ActionM ()
jsonResponse e = WS.json e