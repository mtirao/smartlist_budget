{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module InvoiceController(getInvoice, createInvoice, removeInvoice, updateInvoice) where

import InvoiceDTO
import Invoice
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

--- Invoice
getInvoice userId conn =  do
                            auth <- header "Authorization"
                            let token =  decodeAuthHdr auth
                            result <- liftIO $ findInvoice userId conn
                            invoiceServiceWithBody token result
                
createInvoice body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe InvoiceDTO)
                            let token =  decodeAuthHdr auth
                            case profile of
                                Nothing -> status badRequest400
                                Just a -> do 
                                            result <- liftIO $ insertInvoice a conn
                                            invoiceService token result
                                            
                                            
removeInvoice conn = status unauthorized401

updateInvoice body conn = status unauthorized401 

-- Services
invoiceService Nothing result = do 
                                jsonResponse (ErrorMessage "Invalid token payload")
                                status unauthorized401
invoiceService(Just token) result = validateToken token responseData
                            where responseData = case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "Tender not found")
                                            status badRequest400
                                    Right [a] -> status noContent204

invoiceServiceWithBody Nothing result = do 
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status unauthorized401
invoiceServiceWithBody (Just token) result = validateToken token responseData
                                    where responseData =  case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "Tender not found")
                                                        status badRequest400
                                                Right [a] -> jsonResponse $ toInvoiceDTO a

