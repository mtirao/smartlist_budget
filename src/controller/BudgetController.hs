{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module BudgetController(getBudget, createBudget, removeBudget, updateBudget) where

import BudgetDTO
import Budget
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
import Repository

--- Budget
getBudget conn =  do
                    auth <- header "Authorization"
                    let token =  decodeAuthHdr auth
                    payload <- liftIO $ validateToken auth
                    userId <- liftIO $ tokenUserID payload
                    result <- liftIO (findObject (toStrict userId) conn :: IO [Maybe BudgetDTO])
                    selectItems result conn
                
createBudget body conn =  do
                            auth <- header "Authorization"
                            b <- body
                            let tender = (decode b :: Maybe BudgetDTO)
                            let token =  decodeAuthHdr auth
                            payload <- liftIO $ validateToken auth
                            createObject (DTOBudget tender) token conn

                                            
removeBudget conn = status unauthorized401

updateBudget body conn = status unauthorized401 

