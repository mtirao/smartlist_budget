{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Tender where

import Control.Monad.IO.Class
import Data.Text (Text, unpack, pack)
import Data.UUID
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Hardcoded

import TenderDTO
import Data.UUID.V1 (nextUUID)

-- Rel8 Schemma Definitions
data Tender f = Tender
    {tenderIdT :: Column f ( Maybe UUID)
    , tenderTypeT :: Column f Text
    , tenderNumberT :: Column f Text
    , tenderAliasT :: Column f Text
    , tenderUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Tender f)

tenderSchema :: TableSchema (Tender Name)
tenderSchema = TableSchema
    { name = "tenders"
    , schema = Nothing
    , columns = Tender
        { tenderIdT = "id"
        , tenderTypeT = "type"
        , tenderNumberT = "number"
        , tenderAliasT = "alias"
        , tenderUserIdT = "user_id"
        }
    }

-- Functions
--GET
findTender :: Text -> Connection -> IO (Either QueryError [Tender Result])
findTender userId = run (statement () (select3 userId) )

select3 :: Text -> Statement () [Tender Result]
select3  userId = select $ do
                    t <- each tenderSchema
                    where_ (t.tenderUserIdT ==. lit userId)
                    pure t

-- INSERT
insertTender :: TenderDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
insertTender t u i = run (statement () (insert1 t u i))

insert1 :: TenderDTO -> Text -> Maybe UUID ->Statement () [Maybe UUID]
insert1 t u i = insert $ Insert
            { into = tenderSchema
            , rows = values [ Tender (lit i) (lit t.tenderType) (lit t.tenderNumber) (lit t.tenderAlias) (lit u)]
            , returning = Projection (.tenderIdT)
            , onConflict = Abort
            }


-- DELETE
deleteTender :: TenderDTO -> Connection -> IO (Either QueryError [Maybe UUID])
deleteTender u conn = do
                        run (statement () (delete1 u.tenderId )) conn

delete1 :: Maybe UUID -> Statement () [Maybe UUID]
delete1 u  = delete $ Delete
            { from = tenderSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.tenderIdT ==. lit u
            , returning = Projection (.tenderIdT)
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
toTenderDTO :: Tender Result -> TenderDTO
toTenderDTO t = TenderDTO Nothing t.tenderTypeT t.tenderNumberT t.tenderAliasT
