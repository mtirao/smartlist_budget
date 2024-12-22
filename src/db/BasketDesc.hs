{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module BasketDesc where

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

import BasketDescDTO
import Data.UUID.V1 (nextUUID)

-- Rel8 Schemma Definitions
data BasketDesc f = BasketDesc
    {basketDescIdT :: Column f ( Maybe UUID)
    , basketDescBasketIdT :: Column f UUID
    , basketDescDateT :: Column f Int64
    , basketDescItemIdT :: Column f UUID
    , basketDescLatT :: Column f Float
    , basketDescLonT :: Column f Float
    , basketDescPriceT :: Column f Float
    , basketDescUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (BasketDesc f)

basketDescSchema :: TableSchema (BasketDesc Name)
basketDescSchema = TableSchema
    { name = "basket_descriptions"
    , schema = Nothing
    , columns = BasketDesc
        { basketDescIdT = "id"
        , basketDescBasketIdT = "basket_id"
        , basketDescDateT = "date"
        , basketDescItemIdT = "item_id"
        , basketDescLatT = "lat"
        , basketDescLonT = "lon"
        , basketDescPriceT = "price"
        , basketDescUserIdT = "user_id"
        }
    }


-- Functions
--GET
findBasketDesc :: Text -> Connection -> IO (Either QueryError [BasketDesc Result])
findBasketDesc userId conn = do
                            let query = select $ do
                                            i <- each basketDescSchema
                                            where_ (i.basketDescUserIdT ==. lit userId)
                                            return i
                            run (statement () query ) conn

-- INSERT
insertBasketDesc :: BasketDescDTO -> Text -> Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
insertBasketDesc p u i = run (statement () (insert1 p u i))

insert1 :: BasketDescDTO -> Text -> Maybe UUID -> Statement () [Maybe UUID]
insert1 t u i = insert $ Insert
            { into = basketDescSchema
            , rows = values [ BasketDesc (lit i) (lit t.basketDescBasketId) (lit t.basketDescDate) (lit t.basketDescItemId) (lit t.basketDescLat) (lit t.basketDescLon) (lit t.basketDescPrice) (lit u)]
            , returning = Projection (.basketDescIdT)
            , onConflict = Abort
            }

-- DELETE
deleteBasketDesc :: Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
deleteBasketDesc u = run (statement () (delete1 u ))

delete1 :: Maybe UUID -> Statement () [Maybe UUID]
delete1 u  = delete $ Delete
            { from = basketDescSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.basketDescIdT ==. lit u
            , returning = Projection (.basketDescIdT)
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
toBasketDescDTO :: BasketDesc Result -> BasketDescDTO
toBasketDescDTO t = BasketDescDTO t.basketDescIdT t.basketDescBasketIdT t.basketDescDateT t.basketDescItemIdT t.basketDescLatT t.basketDescLonT t.basketDescPriceT