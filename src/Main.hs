module Main where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal as TI
import Data.ByteString.Conversion.To
import Data.ByteString.Internal
import Data.ByteString.Lazy.Internal
import Data.Pool(createPool)
import Data.ByteString.Lazy (fromStrict)

import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status
import Network.Wai (Request, pathInfo)
import Network.Wai.Middleware.HttpAuth

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

import Control.Monad.IO.Class

import Connection
import TenderController

main :: IO ()
main = do
    -- let tlsConfig = tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
    --    config    = setPort 3443 defaultSettings
    -- pool <- createPool (getConnection) close 1 40 10
    Right connection <- getConnection
    scotty 3000 $ do 
        middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
        middleware logStdout

        -- Tender
        get "/api/wanaka/profile/:id" $ do  
                                        idd <- param "id" :: ActionM TL.Text
                                        getTender (TI.pack (TL.unpack idd)) connection
        post "/api/wanaka/profile" $ createTender body connection
        delete "/api/wanaka/profile" $ removeTender connection
        post "/api/wanaka/profile" $ updateTender body connection