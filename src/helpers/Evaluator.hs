{-# LANGUAGE OverloadedStrings #-}

module Evaluator where

import Data.Aeson
import Data.Int (Int32, Int64)
import Data.Text.Lazy
import Data.Text.Internal.Lazy
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BL 

import Control.Monad.IO.Class

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)
import Network.HTTP.Types.Status
import Views ( jsonResponse )

import Payload
import ErrorMessage

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt), JwsHeader(JwsHeader))

-- Convert apply a func to Maybe a and create an ActionT result
liftMaybe :: Maybe t -> (t -> ActionT IO ()) -> ActionT IO ()
liftMaybe Nothing func = do
                            jsonResponse (ErrorMessage "Invalid token payload")
                            status badRequest400
liftMaybe (Just value) func = func value


-- Decode access token from header
tokenFromHeader :: (Text, Text) -> BI.ByteString
tokenFromHeader (typ, token) = BL.toStrict $ TL.encodeUtf8 token 

convertToPayload :: BI.ByteString -> Maybe Payload
convertToPayload t = ( decode $  BL.packChars $ BI.unpackChars t ) :: Maybe Payload

decodeAuthHdr :: Maybe Text -> Maybe Payload
decodeAuthHdr Nothing = Nothing
decodeAuthHdr (Just a) = (decodeToken $ breakOnEnd " " a ) :: Maybe Payload

decodeToken :: (Text, Text) -> Maybe Payload
decodeToken t = case token of 
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where 
                    token = hmacDecode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" $ tokenFromHeader t

nanosSinceEpoch :: NominalDiffTime -> Int64
nanosSinceEpoch = floor  . nominalDiffTimeToSeconds

secondsSinceEpoch :: NominalDiffTime -> Int64
secondsSinceEpoch = nanosSinceEpoch

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = secondsSinceEpoch u + 864000

refreshTokenExp :: NominalDiffTime -> Int64
refreshTokenExp u = secondsSinceEpoch u + 864000 * 2

toInt64 :: NominalDiffTime -> Int64
toInt64 = secondsSinceEpoch

validateToken :: Payload -> ActionT IO () -> ActionT IO ()
validateToken token action = do 
                        curTime <- liftIO getPOSIXTime
                        if tokenExperitionTime token >= toInt64 curTime then 
                            action
                        else do
                            jsonResponse (ErrorMessage "Token expired")
                            status unauthorized401 