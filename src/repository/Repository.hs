{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Repository where

import Control.Monad.IO.Class
import Data.Text (Text, unpack, pack)
import Data.UUID
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Hardcoded

import TenderDTO
import Tender

import ItemDTO
import Item

import InvoiceDTO
import Invoice

import Budget
import BudgetDTO

import Basket
import BasketDTO

import BasketDesc
import BasketDescDTO

import Data.UUID.V1 (nextUUID)

class Repository a where
    insertObject :: a -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])

instance Repository TenderDTO where 
    insertObject :: TenderDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
    insertObject = insertTender

instance Repository ItemDTO where 
    insertObject :: ItemDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
    insertObject = insertItem

instance Repository InvoiceDTO where 
    insertObject :: InvoiceDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
    insertObject = insertInvoice

instance Repository BasketDTO where 
    insertObject :: BasketDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
    insertObject = insertBasket

instance Repository BasketDescDTO where 
    insertObject :: BasketDescDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
    insertObject = insertBasketDesc
