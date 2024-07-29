{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

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

-- Rel8 Schemma Definitions
data Tender f = Tender
    {tenderIdT :: Column f UUID
    , tenderTypeT :: Column f Text
    , tenderNumberT :: Column f Text
    , tenderAliasT :: Column f Text
    , tenderUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Tender f)

tenderSchema :: TableSchema (Tender Name)
tenderSchema = TableSchema
    { name = "tender"
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
findTender userId conn = do
                            let query = select $ do
                                            t <- each tenderSchema
                                            where_ $ (t.tenderUserIdT ==. lit userId)
                                            return t
                            run (statement () query ) conn

-- INSERT
insertTender :: TenderDTO -> Connection -> IO (Either QueryError [UUID])
insertTender p  conn = run (statement () (insert1 p)) conn

insert1 :: TenderDTO -> Statement () [UUID]
insert1 t = insert $ Insert
            { into = tenderSchema
            , rows = values [ Tender (lit $ t.tenderId) (lit $ t.tenderType) (lit $ t.tenderNumber) (lit $ t.tenderAlias) (lit $ t.tenderUserId)]
            , returning = Projection (.tenderIdT)
            , onConflict = Abort
            }

-- DELETE
deleteProfile :: UUID -> Connection -> IO (Either QueryError [UUID])
deleteProfile u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: UUID -> Statement () [UUID]
delete1 u  = delete $ Delete
            { from = tenderSchema
            , using = pure ()
            , deleteWhere = \t ui -> (ui.tenderIdT ==. lit u)
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
toTenderDTO t = TenderDTO (t.tenderIdT) (t.tenderTypeT) (t.tenderNumberT) (t.tenderAliasT) (t.tenderUserIdT)

