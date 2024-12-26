{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module BasketDescDTO where

import Data.Int (Int32, Int64)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson
import Data.UUID

-- Login Response
data BasketDescDTO = BasketDescDTO
    { basketDescId :: Maybe UUID
    , basketDescBasketId :: Maybe UUID
    , basketDescDate :: Int64
    , basketDescItemId :: Maybe UUID
    , basketDescLat :: Float
    , basketDescLon :: Float
    , basketDescPrice :: Float
    } deriving (Show)
 
instance ToJSON BasketDescDTO where
    toJSON BasketDescDTO {..} = object [
            "id" .= basketDescBasketId,
            "basket_id" .= basketDescBasketId,
            "date" .= basketDescDate,
            "item_id" .= basketDescItemId,
            "lat" .= basketDescLat,
            "lon" .= basketDescLon,
            "price" .= basketDescPrice
        ]

instance FromJSON BasketDescDTO where
    parseJSON (Object v) = BasketDescDTO <$>
        v .:  "id" <*>
        v .:  "basket_id" <*>
        v .:  "date" <*>
        v .:  "item_id" <*>
        v .:  "lat" <*>
        v .:  "lon" <*>
        v .:  "price"