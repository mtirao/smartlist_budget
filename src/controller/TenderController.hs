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

--- Tender
getTender conn =  do
                    auth <- header "Authorization"
                    payload <- liftIO $ validateToken auth
                    case payload of 
                        Nothing -> do 
                                    jsonResponse (ErrorMessage "Invalid token payload")
                                    status unauthorized401
                        Just token -> do 
                                        result <- liftIO $ findTender (toStrict token.user) conn
                                        case result of
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "Tender not found")
                                                    status badRequest400
                                            Right a -> jsonResponse $ Prelude.map toTenderDTO a
                
createTender body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let tender = (decode b :: Maybe TenderDTO)
                            let token =  decodeAuthHdr auth
                            payload <- liftIO $ validateToken auth
                            createObject tender token conn
                            -- uuid <- liftIO nextUUID
                            -- case (profile, payload) of
                            --     (Just a, Just token) -> do 
                            --                 result <- liftIO $ insertTender a (toStrict token.user) uuid conn
                            --                 case result of
                            --                     Left a ->  do
                            --                             jsonResponse (ErrorMessage "Query Error")
                            --                             status badRequest400
                            --                     Right [] -> do
                            --                             jsonResponse (ErrorMessage "Tender not found")
                            --                             status badRequest400
                            --                     Right a  -> status noContent204
                            --     (Nothing, Nothing) -> status badRequest400
                            --     (_, Nothing) -> do 
                            --                     jsonResponse (ErrorMessage "Token Invalid")
                            --                     status unauthorized401
                            --     (Nothing, _) -> do 
                            --                     jsonResponse (ErrorMessage "Profile not found")
                            --                     status badRequest400
                                                
                                            
                                            
removeTender conn = status unauthorized401

updateTender body conn = status unauthorized401 
