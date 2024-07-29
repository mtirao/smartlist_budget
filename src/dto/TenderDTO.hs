{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module TenderDTO where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson
import Data.UUID

-- Login Response
data TenderDTO = TenderDTO
    { tenderId :: UUID
    , tenderType :: T.Text
    , tenderNumber :: T.Text
    , tenderAlias :: T.Text
    , tenderUserId :: T.Text
    } deriving (Show)
 
instance ToJSON TenderDTO where
    toJSON TenderDTO {..} = object [
            "id" .= tenderId,
            "type" .= tenderType,
            "number" .= tenderNumber,
            "alias" .= tenderAlias,
            "user_id" .= tenderUserId
        ]

instance FromJSON TenderDTO where
    parseJSON (Object v) = TenderDTO <$>
        v .:  "id" <*>
        v .:  "type" <*>
        v .:  "number" <*>
        v .:  "alias" <*>
        v .: "user_id"

-- Getters
-- getTenderType :: TenderDTO -> T.Text
-- getTenderType a = T.pack (TL.unpack (tenderType a))

-- getTenderNumber :: TenderDTO -> T.Text
-- getTenderNumber a = T.pack (TL.unpack (tenderNumber a))

-- getTenderAlias :: TenderDTO -> T.Text
-- getTenderAlias a = T.pack (TL.unpack (tenderAlias a))