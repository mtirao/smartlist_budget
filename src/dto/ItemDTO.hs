{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ItemDTO where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson
import Data.UUID

-- Login Response
data ItemDTO = ItemDTO
    { itemId :: Maybe UUID
    , itemCategory :: T.Text
    , itemName :: T.Text
    , itemSku :: T.Text
    , itemUserId :: T.Text
    } deriving (Show)
 
instance ToJSON ItemDTO where
    toJSON ItemDTO {..} = object [
            "id" .= itemId,
            "category" .= itemCategory,
            "name" .= itemName,
            "sku" .= itemSku,
            "user_id" .= itemUserId
        ]

instance FromJSON ItemDTO where
    parseJSON (Object v) = ItemDTO <$>
        v .:  "id" <*>
        v .:  "category" <*>
        v .:  "name" <*>
        v .:  "sku" <*>
        v .: "user_id"