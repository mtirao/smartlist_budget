{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module TenderController(getTender, createTender, removeTender, updateTender) where

import TenderDTO
import Views ( jsonResponse )
import Tender
import Payload
import ErrorMessage

import Evaluator

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class
import Network.HTTP.Types.Status

import Data.Aeson
import Data.UUID
import Data.Text.Lazy
import Data.Text.Internal.Lazy

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)
import Data.Text.Internal.Encoding.Utf32 (validate)
import Data.UUID.V1 (nextUUID)
import Database.PostgreSQL.Simple.ToField (Action)

import Hasql.Connection (Connection)

import Service
import Repository

--- Tender
getTender :: Connection -> ActionT IO ()
getTender conn =  do
                    auth <- header "Authorization"
                    let token =  decodeAuthHdr auth
                    payload <- liftIO $ validateToken auth
                    userId <- liftIO $ tokenUserID payload
                    result <- liftIO (findObject (toStrict userId) conn :: IO [Maybe TenderDTO])
                    selectItems result conn
                
createTender body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let tender = (decode b :: Maybe TenderDTO)
                            let token =  decodeAuthHdr auth
                            createObject (DTOTender tender) token conn
                                            
                                            
removeTender conn = status unauthorized401

updateTender body conn = status unauthorized401 
