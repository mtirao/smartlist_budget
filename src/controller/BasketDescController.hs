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
import Data.Text.Lazy
import Data.Text.Internal.Lazy
import Data.UUID.V1 (nextUUID)

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)

--- BasketDesc
getBasketDesc conn =  do
                    auth <- header "Authorization"
                    payload <- liftIO $ validateToken auth
                    case payload of 
                        Nothing -> do 
                                    jsonResponse (ErrorMessage "Invalid token payload")
                                    status unauthorized401
                        Just token -> do 
                                        result <- liftIO $ findBasketDesc (toStrict token.user) conn
                                        case result of
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "BasketDesc not found")
                                                    status badRequest400
                                            Right [a] -> jsonResponse $ toBasketDescDTO a
                
createBasketDesc body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe BasketDescDTO)
                            let token =  decodeAuthHdr auth
                            payload <- liftIO $ validateToken auth
                            uuid <- liftIO nextUUID
                            case (profile, payload) of
                                (Just a, Just token) -> do 
                                            result <- liftIO $ insertBasketDesc a (toStrict token.user) uuid conn
                                            case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "BasketDesc not found")
                                                        status badRequest400
                                                Right a  -> status noContent204
                                (Nothing, Nothing) -> status badRequest400
                                (_, Nothing) -> status badRequest400
                                (Nothing, _) -> status badRequest400
                                            
                                            
removeBasketDesc conn = status unauthorized401

updateBasketDesc body conn = status unauthorized401 

