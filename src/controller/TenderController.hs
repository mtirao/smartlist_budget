module TenderController(getTender, createTender, deleteTender, updateTender) where

import TenderDTO
import Views ( jsonResponse )
import Tender
import ErrorMessage

import Evaluator

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class
import Network.HTTP.Types.Status

import Data.Aeson

import Data.Time.Clock.POSIX

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))

--- Tender
getTender userId conn =  do
                            curTime <- liftIO getPOSIXTime
                            auth <- header "Authorization"
                            let token =  decodeAuthHdr auth
                            result <- liftIO $ findTender userId conn
                            evaluateToken token result
                
createTender body conn =  do
                            curTime <- liftIO getPOSIXTime
                            h <- header "Authorization"
                            b <- body
                            let profile = (decode b :: Maybe TenderDTO)
                            case profile of
                                Nothing -> status badRequest400
                                Just a -> do
                                            result <- liftIO $ insertTender a conn
                                            case result of
                                                    Right [] -> do
                                                            jsonResponse (ErrorMessage "User not found")
                                                            status forbidden403
                                                    Right [a] -> status noContent204

deleteTender conn = status unauthorized401

updateTender body conn = status unauthorized401 

-- Helpers
evaluateToken Nothing result = do 
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status unauthorized401
evaluateToken (Just token) result = do 
                                    curTime <- liftIO getPOSIXTime
                                    if tokenExperitionTime token >= toInt64 curTime then 
                                        case result of
                                                Right [] -> do
                                                        jsonResponse (ErrorMessage "Tender not found")
                                                        status badRequest400
                                                Right [a] -> jsonResponse $ toTenderDTO a
                                    else do
                                        jsonResponse (ErrorMessage "Token expired")
                                        status unauthorized401     
