{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ErrorMessage where

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

--ErrorMessage
newtype ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]