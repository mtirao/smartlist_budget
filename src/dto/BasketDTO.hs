{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module BasketDTO where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson
import Data.UUID

-- Login Response
data BasketDTO = BasketDTO
    { basketId :: Maybe UUID
    , basketStatus :: T.Text
    , basketTenderId :: UUID
    } deriving (Show)
 
instance ToJSON BasketDTO where
    toJSON BasketDTO {..} = object [
            "id" .= basketId,
            "status" .= basketStatus,
            "tender_id" .= basketTenderId
        ]

instance FromJSON BasketDTO where
    parseJSON (Object v) = BasketDTO <$>
        v .:  "id" <*>
        v .:  "status" <*>
        v .:  "tender_id"