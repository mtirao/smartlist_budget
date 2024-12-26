{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module ServiceHelper where

import Data.Aeson
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty ( status )
import Network.HTTP.Types.Status
import Hasql.Connection (Connection)
import Views
import ErrorMessage

-- Services helpers
selectItems :: ToJSON a =>  [a] -> Connection -> ActionT IO ()
selectItems result conn = do
                            case result of
                                [] -> do
                                        jsonResponse (ErrorMessage "Tender not found")
                                        status badRequest400
                                list -> jsonResponse  list

