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

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))
import GHC.Generics (U1(U1))
import Network.Wreq (responseBody)

--- Item
getItem userId conn =  do
                            auth <- header "Authorization"
                            let token =  decodeAuthHdr auth
                            result <- liftIO $ findItem userId conn
                            itemServiceWithBody token result
                
createItem body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe ItemDTO)
                            let token =  decodeAuthHdr auth
                            case profile of
                                Nothing -> status badRequest400
                                Just a -> do 
                                            result <- liftIO $ insertItem a conn
                                            itemService token result
                                            
                                            
removeItem conn = status unauthorized401

updateItem body conn = status unauthorized401 

-- Services
itemService Nothing result = do 
                                jsonResponse (ErrorMessage "Invalid token payload")
                                status unauthorized401
itemService(Just token) result = validateToken token responseData
                            where responseData = case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "Tender not found")
                                            status badRequest400
                                    Right [a] -> status noContent204

itemServiceWithBody Nothing result = do 
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status unauthorized401
itemServiceWithBody (Just token) result = validateToken token responseData
                                    where responseData =  case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "Tender not found")
                                                        status badRequest400
                                                Right [a] -> jsonResponse $ toItemDTO a

