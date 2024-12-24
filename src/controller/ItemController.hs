{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ItemController(getItem, createItem, removeItem, updateItem) where

import ItemDTO
import Item
import Views ( jsonResponse )
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
import Data.UUID.V1 (nextUUID)

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)
import Service

--- Item
getItem conn =  do
                    auth <- header "Authorization"
                    let token =  decodeAuthHdr auth
                    payload <- liftIO $ validateToken auth
                    selectItem payload conn
                
createItem body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let tender = (decode b :: Maybe ItemDTO)
                            let token =  decodeAuthHdr auth
                            payload <- liftIO $ validateToken auth
                            createObject (DTOItem tender) token conn
                                            
                                            
removeItem conn = status unauthorized401

updateItem body conn = status unauthorized401 

