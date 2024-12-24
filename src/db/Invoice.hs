{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Invoice where

import Control.Monad.IO.Class
import Data.Text (Text, unpack, pack)
import Data.Int (Int32, Int64)
import Data.UUID
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError (QueryError), run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Hardcoded
import Evaluator (emptyQueryError)

import InvoiceDTO
import Data.UUID.V1 (nextUUID)

-- Rel8 Schemma Definitions
data Invoice f = Invoice
    {invoiceIdT :: Column f ( Maybe UUID)
    , invoiceAmountT :: Column f Float
    , invoiceBudgeT :: Column f UUID
    , invoiceDateT :: Column f Int64
    , invoiceNameT :: Column f Text
    , invoiceUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Invoice f)

invoiceSchema :: TableSchema (Invoice Name)
invoiceSchema = TableSchema
    { name = "invoices"
    , schema = Nothing
    , columns = Invoice
        { invoiceIdT = "id"
        , invoiceAmountT = "amount"
        , invoiceBudgeT = "budget"
        , invoiceDateT = "date"
        , invoiceNameT = "name"
        , invoiceUserIdT = "user_id"
        }
    }

-- Functions
--GET
findInvoice :: Text -> Connection -> IO (Either QueryError [Invoice Result])
findInvoice userId conn = do
                            let query = select $ do
                                            i <- each invoiceSchema
                                            where_ (i.invoiceUserIdT ==. lit userId)
                                            return i
                            run (statement () query ) conn

-- INSERT

insertInvoice :: Maybe InvoiceDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
insertInvoice p u i = case p of 
                            Nothing -> return emptyQueryError
                            Just inv -> run (statement () (insert1 inv u i))

insert1 :: InvoiceDTO -> Text -> Maybe UUID -> Statement () [Maybe UUID]
insert1 t u i = insert $ Insert
            { into = invoiceSchema
            , rows = values [ Invoice (lit i) (lit t.invoiceAmount) (lit t.invoiceBudget) (lit t.invoiceDate) (lit t.invoiceName) (lit u)]
            , returning = Projection (.invoiceIdT)
            , onConflict = Abort
            }


-- DELETE
deleteInvoice :: Maybe InvoiceDTO -> Connection -> IO (Either QueryError [Maybe UUID])
deleteInvoice u = case u of 
                    Nothing -> return emptyQueryError
                    Just inv -> run (statement () (delete1 inv.invoiceId ))

delete1 :: Maybe UUID -> Statement () [Maybe UUID]
delete1 u  = delete $ Delete
            { from = invoiceSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.invoiceIdT ==. lit u
            , returning = Projection (.invoiceIdT)
            }

-- UPDATE
-- updateProfile :: Text -> ProfileDTO -> Connection -> IO (Either QueryError [Text])
-- updateProfile u p conn = do
--                         run (statement () (update1 u p)) conn

-- update1 :: Text -> ProfileDTO -> Statement () [Text]
-- update1 u p  = update $ Update
--             { target = profileSchema
--             , from = pure ()
--             , set = \_ row -> Profile (lit $ getCellPhone p) (lit $ getEmail p) (lit $ getFirstName p) (lit $ getLastName p) (lit $ getPhone p) (lit $ getGender p) (lit $ getAddress p) (lit $ getCity p) row.userId
--             , updateWhere = \t ui -> (ui.userId ==. lit u)
--             , returning = Projection (.userId)
--             }

-- Helpers
toInvoiceDTO :: Invoice Result -> InvoiceDTO
toInvoiceDTO t = InvoiceDTO t.invoiceIdT t.invoiceAmountT t.invoiceBudgeT t.invoiceDateT t.invoiceNameT

