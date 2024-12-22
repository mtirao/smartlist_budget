{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module BudgetDTO where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Int (Int32, Int64)
import Data.Aeson
import Data.UUID

-- Login Response
data BudgetDTO = BudgetDTO
    { budgetId :: Maybe UUID
    , budgetAmount :: Float
    , budgetDate :: Int64
    , budgetName :: T.Text
    } deriving (Show)
 
instance ToJSON BudgetDTO where
    toJSON BudgetDTO {..} = object [
            "id" .= budgetId,
            "amount" .= budgetAmount,
            "date" .= budgetDate,
            "name" .= budgetName
        ]

instance FromJSON BudgetDTO where
    parseJSON (Object v) = BudgetDTO <$>
        v .:  "id" <*>
        v .:  "amount" <*>
        v .:  "date" <*>
        v .:  "name"