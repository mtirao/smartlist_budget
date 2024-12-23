{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

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
    deleteObject :: a -> Connection -> IO (Either QueryError [Maybe UUID])

instance Repository TenderDTO where 
    insertObject = insertTender
    deleteObject = deleteTender

instance Repository ItemDTO where 
    insertObject = insertItem
    deleteObject = deleteItem

instance Repository InvoiceDTO where 
    insertObject = insertInvoice
    deleteObject = deleteInvoice

instance Repository BasketDTO where 
    insertObject = insertBasket
    deleteObject = deleteBasket

instance Repository BasketDescDTO where 
    insertObject = insertBasketDesc
    deleteObject = deleteBasketDesc
