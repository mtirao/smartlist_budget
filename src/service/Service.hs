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
import qualified Data.Text as Data.Text.Internal
import Invoice (Invoice)


-- DTOS Definitions
data DTOS = DTOTender (Maybe TenderDTO) 
            | DTOItem (Maybe ItemDTO)
            | DTOInvoice (Maybe InvoiceDTO)
            | DTOBudget (Maybe BudgetDTO)
            | DTOBasket (Maybe BasketDTO)
            | DTOBasketDesc (Maybe BasketDescDTO)

insertDTO :: DTOS -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID]) 
insertDTO b t u c = case b of
                        DTOItem a -> insertObject a (toStrict t) u c
                        DTOTender a -> insertObject a (toStrict t) u c
                        DTOInvoice a -> insertObject a (toStrict t) u c
                        DTOBudget a -> insertObject a (toStrict t) u c
                        DTOBasket a -> insertObject a (toStrict t) u c
                        DTOBasketDesc a -> insertObject a (toStrict t) u c

deleteDTO b c = case b of
                        DTOItem a -> deleteObject a c
                        DTOTender a -> deleteObject a c
                        DTOInvoice a -> deleteObject a c
                        DTOBudget a -> deleteObject a c
                        DTOBasket a -> deleteObject a c
                        DTOBasketDesc a -> deleteObject a c

-- Service Class/Instances definition
class Service a where
    createObject :: a -> Maybe Payload -> Connection -> ActionT IO ()
    removeObject :: a -> Maybe Payload -> Connection -> ActionT IO ()

instance Service DTOS where
    createObject :: DTOS -> Maybe Payload -> Connection -> ActionT IO ()
    createObject dto payload conn = do
        uuid <- liftIO nextUUID
        case (dto, payload) of
            (obj, Just token) -> do
                        result <- liftIO $ insertDTO obj token.user uuid conn
                        case result of
                            Left a ->  do
                                    jsonResponse (ErrorMessage "Query Error")
                                    status badRequest400
                            Right [] -> do
                                    jsonResponse (ErrorMessage "Tender not found")
                                    status badRequest400
                            Right a  -> status noContent204
            (_, Nothing) -> do
                            jsonResponse (ErrorMessage "Token Invalid")
                            status unauthorized401

    removeObject :: DTOS -> Maybe Payload -> Connection -> ActionT IO ()
    removeObject dto payload conn = case payload of
                                    Just token ->  do
                                        result <- liftIO $ deleteDTO dto conn
                                        case result of 
                                            Left a -> do
                                                        jsonResponse (ErrorMessage "Query Error")
                                                        status badRequest400
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "Tender not found")
                                                    status badRequest400
                                            Right a  -> status noContent204
                                    Nothing -> do
                                                jsonResponse (ErrorMessage "Token Invalid")
                                                status unauthorized401



