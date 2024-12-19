{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Basket where

import Control.Monad.IO.Class
import Data.Text (Text, unpack, pack)
import Data.Int (Int32, Int64)
import Data.UUID
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Hardcoded

import BasketDTO
import Data.UUID.V1 (nextUUID)


-- Rel8 Schemma Definitions
data Basket f = Basket
    {basketIdT :: Column f ( Maybe UUID)
    , basketStatusT :: Column f Text
    , basketTenderIdT :: Column f UUID
    , basketUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Basket f)

basketSchema :: TableSchema (Basket Name)
basketSchema = TableSchema
    { name = "baskets"
    , schema = Nothing
    , columns = Basket
        { basketIdT = "id"
        , basketStatusT = "status"
        , basketTenderIdT = "tender_id"
        , basketUserIdT = "user_id"
        }
    }

-- Functions
--GET
findBasket :: Text -> Connection -> IO (Either QueryError [Basket Result])
findBasket userId conn = do
                            let query = select $ do
                                            i <- each basketSchema
                                            where_ (i.basketUserIdT ==. lit userId)
                                            return i
                            run (statement () query ) conn

-- INSERT
insertBasket :: BasketDTO -> Connection -> IO (Either QueryError [Maybe UUID])
insertBasket p = run (statement () (insert1 p))

insert1 :: BasketDTO -> Statement () [Maybe UUID]
insert1 t = insert $ Insert
            { into = basketSchema
            , rows = values [ Basket (lit Nothing) (lit t.basketStatus) (lit t.basketTenderId) (lit t.basketUserId)]
            , returning = Projection (.basketIdT)
            , onConflict = Abort
            }


-- DELETE
deleteBasket :: Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
deleteBasket u conn = run (statement () (delete1 u )) conn

delete1 :: Maybe UUID -> Statement () [Maybe UUID]
delete1 u  = delete $ Delete
            { from = basketSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.basketIdT ==. lit u
            , returning = Projection (.basketIdT)
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
toBasketDTO :: Basket Result -> BasketDTO
toBasketDTO t = BasketDTO t.basketIdT t.basketStatusT t.basketTenderIdT t.basketUserIdT