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
import BasketController
import BasketDescController
import BudgetController
import InvoiceController
import ItemController

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
        get "/api/v1/tender" $ getTender connection
        post "/api/v1/tender" $ createTender body connection
        delete "/api/v1/tender/:id" $ do idd <- param "id" :: ActionM TL.Text 
                                         removeTender idd connection
        put "/api/v1/tender" $ updateTender body connection

        -- Item
        get "/api/v1/item" $ getItem connection
        post "/api/v1/item" $ createItem body connection
        delete "/api/v1/item/:id" $ do 
                                        idd <- param "id" :: ActionM TL.Text 
                                        removeItem idd connection
        put "/api/v1/item" $ updateItem body connection

        -- Invoice
        get "/api/v1/invoice" $ getInvoice connection
        post "/api/v1/invoice" $ createInvoice body connection
        delete "/api/v1/invoice/:id" $  do 
                                            idd <- param "id" :: ActionM TL.Text 
                                            removeInvoice idd connection
        put "/api/v1/invoice" $ updateInvoice body connection

        -- Budget
        get "/api/v1/budget" $ getBudget connection
        post "/api/v1/budget" $ createBudget body connection
        delete "/api/v1/budget/:id" $ do 
                                        idd <- param "id" :: ActionM TL.Text 
                                        removeBudget idd connection
        put "/api/v1/budget" $ updateBudget body connection

        -- BasketDesc
        get "/api/v1/basket-description" $ getBasketDesc connection
        post "/api/v1/basket-description" $ createBasketDesc body connection
        delete "/api/v1/basket-description/:id" $ do idd <- param "id" :: ActionM TL.Text 
                                                     removeBasketDesc idd connection
        put "/api/v1/basket-description" $ updateBasketDesc body connection

        -- Bakset
        get "/api/v1/basket" $ getBasket connection
        post "/api/v1/basket" $ createBasket body connection
        delete "/api/v1/basket/:id" $ do idd <- param "id" :: ActionM TL.Text 
                                         removeBasket idd connection
        put "/api/v1/basket" $ updateBasket body connection