{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Budget where

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

import BudgetDTO
import Data.UUID.V1 (nextUUID)


-- Rel8 Schemma Definitions
data Budget f = Budget
    {budgetIdT :: Column f ( Maybe UUID)
    , budgetAmountT :: Column f Float
    , budgetDateT :: Column f Int64
    , budgetNameT :: Column f Text
    , budgetUserIdT :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Budget f)

budgetSchema :: TableSchema (Budget Name)
budgetSchema = TableSchema
    { name = "budgets"
    , schema = Nothing
    , columns = Budget
        { budgetIdT = "id"
        , budgetAmountT = "amount"
        , budgetDateT = "date"
        , budgetNameT = "name"
        , budgetUserIdT = "user_id"
        }
    }

-- Functions
--GET
findBudget :: Text -> Connection -> IO (Either QueryError [Budget Result])
findBudget userId conn = do
                            let query = select $ do
                                            i <- each budgetSchema
                                            where_ (i.budgetUserIdT ==. lit userId)
                                            return i
                            run (statement () query ) conn

-- INSERT
insertBudget :: BudgetDTO -> Connection -> IO (Either QueryError [Maybe UUID])
insertBudget p = run (statement () (insert1 p))

insert1 :: BudgetDTO -> Statement () [Maybe UUID]
insert1 t = insert $ Insert
            { into = budgetSchema
            , rows = values [ Budget (lit Nothing) (lit t.budgetAmount) (lit t.budgetDate) (lit t.budgetName) (lit t.budgetUserId)]
            , returning = Projection (.budgetIdT)
            , onConflict = Abort
            }


-- DELETE
deleteBudget :: Maybe UUID -> Connection -> IO (Either QueryError [Maybe UUID])
deleteBudget u conn = run (statement () (delete1 u )) conn

delete1 :: Maybe UUID -> Statement () [Maybe UUID]
delete1 u  = delete $ Delete
            { from = budgetSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.budgetIdT ==. lit u
            , returning = Projection (.budgetIdT)
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
toBudgetDTO :: Budget Result -> BudgetDTO
toBudgetDTO t = BudgetDTO t.budgetIdT t.budgetAmountT t.budgetDateT t.budgetNameT t.budgetUserIdT