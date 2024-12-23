{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Service where

import TenderDTO
import Views ( jsonResponse )
import Tender
import Payload
import ErrorMessage

import Evaluator

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class
import Network.HTTP.Types.Status

import Data.Aeson
import Data.UUID
import Data.Text.Lazy
import Data.Text.Internal.Lazy

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)
import Data.Text.Internal.Encoding.Utf32 (validate)
import Data.UUID.V1 (nextUUID)
import Database.PostgreSQL.Simple.ToField (Action)

import Hasql.Connection (Connection)

import Repository
import ItemDTO (ItemDTO)
import InvoiceDTO (InvoiceDTO)
import BudgetDTO (BudgetDTO)
import BasketDTO (BasketDTO)
import BasketDescDTO (BasketDescDTO)
import Hasql.Session (QueryError)

class Service a where
    createObject :: a -> Maybe Payload -> Connection -> ActionT IO ()

instance Service (Maybe TenderDTO) where
    createObject :: Maybe TenderDTO -> Maybe Payload -> Connection -> ActionT IO ()
    createObject tender payload conn = do
        uuid <- liftIO nextUUID
        case (tender, payload) of
            (Just obj, Just token) -> do 
                        result <- liftIO $ insertObject obj (toStrict token.user) uuid conn
                        case result of
                            Left a ->  do
                                    jsonResponse (ErrorMessage "Query Error")
                                    status badRequest400
                            Right [] -> do
                                    jsonResponse (ErrorMessage "Tender not found")
                                    status badRequest400
                            Right a  -> status noContent204
            (Nothing, Nothing) -> status badRequest400
            (_, Nothing) -> do 
                            jsonResponse (ErrorMessage "Token Invalid")
                            status unauthorized401
            (Nothing, _) -> do 
                            jsonResponse (ErrorMessage "Profile not found")
                            status badRequest400
