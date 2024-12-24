{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

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
    findObject :: Text -> Connection -> IO [a]

instance Repository (Maybe TenderDTO) where
    insertObject = insertTender
    deleteObject = deleteTender
    findObject a conn = do
                            result <- findTender a conn
                            case result of
                                Right [] -> return []
                                Right a -> return $ map (Just . toTenderDTO) a


instance Repository (Maybe ItemDTO) where
        insertObject = insertItem
        deleteObject = deleteItem
        findObject a conn = do
                            result <- findItem a conn
                            case result of
                                Right [] -> return []
                                Right a -> return $ map (Just . toItemDTO) a

instance Repository (Maybe InvoiceDTO) where
    insertObject = insertInvoice
    deleteObject = deleteInvoice
    findObject a conn = do
                            result <- findInvoice a conn
                            case result of
                                Right [] -> return []
                                Right a -> return $ map (Just . toInvoiceDTO) a

instance Repository (Maybe BasketDTO) where
    insertObject = insertBasket
    deleteObject = deleteBasket
    findObject a conn = do
                            result <- findBasket a conn
                            case result of
                                Right [] -> return []
                                Right a -> return $ map (Just . toBasketDTO) a

instance Repository (Maybe BasketDescDTO) where
    insertObject = insertBasketDesc
    deleteObject = deleteBasketDesc
    findObject a conn = do
                            result <- findBasketDesc a conn
                            case result of
                                Right [] -> return []
                                Right a -> return $ map (Just . toBasketDescDTO) a

instance Repository (Maybe BudgetDTO) where
    insertObject = insertBudget
    deleteObject = deleteBudget
    findObject a conn = do
                            result <- findBudget a conn
                            case result of
                                Right [] -> return []
                                Right a -> return $ map (Just . toBudgetDTO) a