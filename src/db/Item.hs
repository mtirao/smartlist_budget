{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Item where

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

import ItemDTO
import Data.UUID.V1 (nextUUID)

-- Rel8 Schemma Definitions
data Item f = Item
    {itemIdT :: Column f ( Maybe UUID)
    , itemCategoryT :: Column f Text
    , itemNameT :: Column f Text
    , itemSkuT :: Column f Text
    , itemUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Item f)

itemSchema :: TableSchema (Item Name)
itemSchema = TableSchema
    { name = "items"
    , schema = Nothing
    , columns = Item
        { itemIdT = "id"
        , itemCategoryT = "category"
        , itemNameT = "name"
        , itemSkuT = "sku"
        , itemUserIdT = "user_id"
        }
    }

-- Functions
--GET
findItem :: Text -> Connection -> IO (Either QueryError [Item Result])
findItem userId conn = do
                            let query = select $ do
                                            i <- each itemSchema
                                            where_ (i.itemUserIdT ==. lit userId)
                                            return i
                            run (statement () query ) conn

-- INSERT
insertItem :: ItemDTO -> Connection -> IO (Either QueryError [Maybe UUID])
insertItem p = run (statement () (insert1 p))

insert1 :: ItemDTO -> Statement () [Maybe UUID]
insert1 t = insert $ Insert
            { into = itemSchema
            , rows = values [ Item (lit Nothing) (lit t.itemCategory) (lit t.itemName) (lit t.itemSku) (lit t.itemUserId)]
            , returning = Projection (.itemIdT)
            , onConflict = Abort
            }


-- DELETE
deleteItem :: Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
deleteItem u conn = run (statement () (delete1 u )) conn

delete1 :: Maybe UUID -> Statement () [Maybe UUID]
delete1 u  = delete $ Delete
            { from = itemSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.itemIdT ==. lit u
            , returning = Projection (.itemIdT)
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
toItemDTO :: Item Result -> ItemDTO
toItemDTO t = ItemDTO t.itemIdT t.itemCategoryT t.itemNameT t.itemSkuT t.itemUserIdT

