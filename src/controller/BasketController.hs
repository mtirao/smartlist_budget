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

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)

--- Basket
getBasket userId conn =  do
                            auth <- header "Authorization"
                            let token =  decodeAuthHdr auth
                            result <- liftIO $ findBasket userId conn
                            basketServiceWithBody token result
                
createBasket body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe BasketDTO)
                            let token =  decodeAuthHdr auth
                            case profile of
                                Nothing -> status badRequest400
                                Just a -> do 
                                            result <- liftIO $ insertBasket a conn
                                            basketService token result
                                            
                                            
removeBasket conn = status unauthorized401

updateBasket body conn = status unauthorized401 

-- Services
basketService Nothing result = do 
                                jsonResponse (ErrorMessage "Invalid token payload")
                                status unauthorized401
basketService(Just token) result = validateToken token responseData
                            where responseData = case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "Tender not found")
                                            status badRequest400
                                    Right [a] -> status noContent204

basketServiceWithBody Nothing result = do 
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status unauthorized401
basketServiceWithBody (Just token) result = validateToken token responseData
                                    where responseData =  case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "Tender not found")
                                                        status badRequest400
                                                Right [a] -> jsonResponse $ toBasketDTO a

