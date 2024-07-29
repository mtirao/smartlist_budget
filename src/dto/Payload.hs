{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Payload where

import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import GHC.Generics
import Data.Time.LocalTime
import Data.Int
import Data.Time.Clock.POSIX
import Data.Time
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import qualified Data.Maybe
import qualified Data.ByteString.Lazy.Internal as BI

data Payload = Payload
    {
        user :: Text,
        exp :: Int64
    } deriving (Show)

instance ToJSON Payload where
    toJSON (Payload user exp) = object
        [
            "user" .= user,
            "exp" .= exp
        ]                           

instance FromJSON Payload where
    parseJSON (Object v) = Payload <$>
        v .: "user" <*>
        v .: "exp"

tokenExperitionTime :: Payload -> Int64
tokenExperitionTime (Payload u e) = e

tokenUserId :: Payload -> Text
tokenUserId (Payload u e) = u