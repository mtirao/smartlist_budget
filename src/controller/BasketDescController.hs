{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module BasketDescController(getBasketDesc, createBasketDesc, removeBasketDesc, updateBasketDesc) where

import BasketDescDTO
import BasketDesc
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

--- BasketDesc
getBasketDesc userId conn =  do
                            auth <- header "Authorization"
                            let token =  decodeAuthHdr auth
                            result <- liftIO $ findBasketDesc userId conn
                            basketDescServiceWithBody token result
                
createBasketDesc body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe BasketDescDTO)
                            let token =  decodeAuthHdr auth
                            case profile of
                                Nothing -> status badRequest400
                                Just a -> do 
                                            result <- liftIO $ insertBasketDesc a conn
                                            basketDescService token result
                                            
                                            
removeBasketDesc conn = status unauthorized401

updateBasketDesc body conn = status unauthorized401 

-- Services
basketDescService Nothing result = do 
                                jsonResponse (ErrorMessage "Invalid token payload")
                                status unauthorized401
basketDescService(Just token) result = validateToken token responseData
                            where responseData = case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "Tender not found")
                                            status badRequest400
                                    Right [a] -> status noContent204

basketDescServiceWithBody Nothing result = do 
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status unauthorized401
basketDescServiceWithBody (Just token) result = validateToken token responseData
                                    where responseData =  case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "Tender not found")
                                                        status badRequest400
                                                Right [a] -> jsonResponse $ toBasketDescDTO a

