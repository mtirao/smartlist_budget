{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module BasketController(getBasket, createBasket, removeBasket, updateBasket) where

import BasketDTO
import Basket
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

--- Basket
getBasket conn =  do
                    auth <- header "Authorization"
                    payload <- liftIO $ validateToken auth
                    case payload of 
                        Nothing -> do 
                                    jsonResponse (ErrorMessage "Invalid token payload")
                                    status unauthorized401
                        Just token -> do 
                                        result <- liftIO $ findBasket (toStrict token.user) conn
                                        case result of
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "Basket not found")
                                                    status badRequest400
                                            Right [a] -> jsonResponse $ toBasketDTO a
                
createBasket body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let tender = (decode b :: Maybe BasketDTO)
                            let token =  decodeAuthHdr auth
                            payload <- liftIO $ validateToken auth
                            createObject (DTOBasket tender) token conn 
                            
                                                             
removeBasket conn = status unauthorized401

updateBasket body conn = status unauthorized401 

