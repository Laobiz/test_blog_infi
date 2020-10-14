-- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.hs` to customize the Types
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
module Generated.Types where

import IHP.HaskellSupport
import IHP.ModelSupport
import CorePrelude hiding (id) 
import Data.Time.Clock 
import qualified Data.Time.Calendar
import qualified Data.List as List 
import qualified Data.ByteString as ByteString 
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField hiding (Field)
import qualified IHP.Controller.Param
import GHC.TypeLits
import Data.UUID (UUID)
import Data.Default
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Data.Proxy
import GHC.Records
import Data.Data
import qualified Data.String.Conversions
import qualified Data.Text.Encoding
import qualified Data.Aeson
import Database.PostgreSQL.Simple.Types (Query (Query), Binary ( .. ))
import qualified Database.PostgreSQL.Simple.Types




data Project'  = Project {id :: (Id' "projects"), meta :: MetaBag} deriving (Eq, Show)
instance InputValue Project where inputValue = IHP.ModelSupport.recordToInputValue
type Project = Project' 

instance FromRow Project where
    fromRow = do
        id <- field
        pure $ Project id def

type instance GetTableName (Project' ) = "projects"
type instance GetModelByTableName "projects" = Project
type instance GetModelName (Project' ) = "Project"

type instance PrimaryKey "projects" = UUID

instance QueryBuilder.FilterPrimaryKey Project where
    filterWhereId id builder =
        builder |> QueryBuilder.filterWhere (#id, id)

instance CanCreate Project where
    create :: (?modelContext :: ModelContext) => Project -> IO Project
    create model = do
        let ModelContext { databaseConnection } = ?modelContext
        result <- Database.PostgreSQL.Simple.query databaseConnection "INSERT INTO projects (id) VALUES (?) RETURNING *" (Only (fieldWithDefault #id model))
        pure (List.head result)
    createMany models = do
        let ModelContext { databaseConnection } = ?modelContext
        Database.PostgreSQL.Simple.query databaseConnection (Query $ "INSERT INTO projects (id) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?)") models)) <> " RETURNING *") (List.concat $ List.map (\model -> [toField (fieldWithDefault #id model)]) models)

instance CanUpdate Project where
    updateRecord model = do
        let ModelContext { databaseConnection } = ?modelContext
        result <- Database.PostgreSQL.Simple.query databaseConnection "UPDATE projects SET id = ? WHERE id = ? RETURNING *" ((fieldWithUpdate #id model, get #id model))
        pure (List.head result)

instance Record Project where
    {-# INLINE newRecord #-}
    newRecord = Project def  def

instance SetField "id" (Project' ) (Id' "projects") where
    {-# INLINE setField #-}
    setField newValue (Project id meta) =
        Project newValue (meta { touchedFields = "id" : touchedFields meta })
instance SetField "meta" (Project' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (Project id meta) =
        Project id newValue
instance UpdateField "id" (Project' ) (Project' ) (Id' "projects") (Id' "projects") where
    {-# INLINE updateField #-}
    updateField newValue (Project id meta) = Project newValue (meta { touchedFields = "id" : touchedFields meta })
instance UpdateField "meta" (Project' ) (Project' ) MetaBag MetaBag where
    {-# INLINE updateField #-}
    updateField newValue (Project id meta) = Project id newValue

