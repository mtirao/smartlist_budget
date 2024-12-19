{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module InvoiceDTO where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Int (Int32, Int64)
import Data.Aeson
import Data.UUID

-- Login Response
data InvoiceDTO = InvoiceDTO
    { invoiceId :: Maybe UUID
    , invoiceAmount :: Float
    , invoiceBudget :: UUID
    , invoiceDate :: Int64
    , invoiceName :: T.Text
    , invoiceUserId :: T.Text
    } deriving (Show)
 
instance ToJSON InvoiceDTO where
    toJSON InvoiceDTO {..} = object [
            "id" .= invoiceId,
            "amount" .= invoiceAmount,
            "budget" .= invoiceBudget,
            "date" .= invoiceDate,
            "name" .= invoiceName,
            "user_id" .= invoiceUserId
        ]

instance FromJSON InvoiceDTO where
    parseJSON (Object v) = InvoiceDTO <$>
        v .:  "id" <*>
        v .:  "amount" <*>
        v .:  "budget" <*>
        v .:  "date" <*>
        v .:  "name" <*>
        v .: "user_id"