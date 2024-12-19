{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)

--- Tender
getTender userId conn =  do
                            auth <- header "Authorization"
                            let token =  decodeAuthHdr auth
                            result <- liftIO $ findTender userId conn
                            tenderServiceWithBody token result
                
createTender body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe TenderDTO)
                            let token =  decodeAuthHdr auth
                            case profile of
                                Nothing -> status badRequest400
                                Just a -> do 
                                            result <- liftIO $ insertTender a conn
                                            tenderService token result
                                            
                                            
removeTender conn = status unauthorized401

updateTender body conn = status unauthorized401 

-- Services
tenderService Nothing result = do 
                                jsonResponse (ErrorMessage "Invalid token payload")
                                status unauthorized401
tenderService(Just token) result = validateToken token responseData
                            where responseData = case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "Tender not found")
                                            status badRequest400
                                    Right [a] -> status noContent204

tenderServiceWithBody Nothing result = do 
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status unauthorized401
tenderServiceWithBody (Just token) result = validateToken token responseData
                                    where responseData =  case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "Tender not found")
                                                        status badRequest400
                                                Right [a] -> jsonResponse $ toTenderDTO a

