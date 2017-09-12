module Database.Sequelize where

import Prelude
import Data.Record.Class (class Subrow)
import Data.Argonaut (Json)
import Data.Nullable (Nullable, toNullable, toMaybe)
import Data.URI (Host)
import Data.URI.Host as Host
import Data.Maybe (Maybe (..))
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1, runEffFn1, EffFn2, runEffFn2, EffFn3, runEffFn3, EffFn4, runEffFn4)
import Control.Monad.Eff.Exception (Error)
import Unsafe.Coerce (unsafeCoerce)



data Dialect
  = Mysql
  | Sqlite
  | Postgres
  | Mssql

instance showDialect :: Show Dialect where
  show Mysql = "mysql"
  show Sqlite = "sqlite"
  show Postgres = "postgres"
  show Mssql = "mssql"


type SequelizeImplParams o =
  { database :: String
  , username :: String
  , password :: String
  , host     :: String
  , dialect  :: String
  , pool     :: { min :: Int, max :: Int, idle :: Int }
  | o
  }

type SequelizeParams o =
  { database :: String
  , username :: String
  , password :: String
  , host     :: Host
  , dialect  :: Dialect
  , pool     :: { min :: Int, max :: Int, idle :: Int }
  | o }

toSequelizeImplParams :: forall o. SequelizeParams o -> SequelizeImplParams o
toSequelizeImplParams x@{host,dialect} =
  x { host = Host.print host
    , dialect = show dialect
    }

type SequelizeParamsO =
  ( storage :: String -- Sqlite
  )


foreign import data SEQUELIZE :: Effect

foreign import data Sequelize :: Type

foreign import sequelizeImpl :: forall eff o
                              . Subrow o SequelizeParamsO
                             => EffFn1 (sequelize :: SEQUELIZE | eff) (SequelizeImplParams o) Sequelize


sequelize :: forall eff o
           . Subrow o SequelizeParamsO
           => SequelizeParams o -> Eff (sequelize :: SEQUELIZE | eff) Sequelize
sequelize = runEffFn1 sequelizeImpl <<< toSequelizeImplParams


foreign import authenticateImpl :: forall eff
                                 . EffFn3 (sequelize :: SEQUELIZE | eff)
                                     (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                     (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                     Sequelize Unit

authenticate :: forall eff. Sequelize -> Aff (sequelize :: SEQUELIZE | eff) Unit
authenticate sql = makeAff \onError onSuccess -> runEffFn3 authenticateImpl (mkEffFn1 onError) (onSuccess unit) sql


foreign import data SequelizeType :: Type

foreign import sqlSTRING :: SequelizeType
foreign import sqlTEXT :: SequelizeType
foreign import sqlBOOLEAN :: SequelizeType
foreign import sqlDATE :: SequelizeType
foreign import sqlINTEGER :: SequelizeType
foreign import sqlFLOAT :: SequelizeType
foreign import sqlDOUBLE :: SequelizeType

foreign import data SequelizeValue :: Type

foreign import sqlNOW :: SequelizeValue


type DefineFieldParams o =
  { "type" :: SequelizeType
  | o }

foreign import data SequelizeDefer :: Type

foreign import sqlPgINITIALLYIMMEDIATE :: SequelizeDefer

type ReferencesParamsO =
  ( deferrable :: SequelizeDefer
  )

type DefineFieldParamsO value references =
  ( allowNull :: Boolean
  , defaultValue :: value
  , unique :: String
  , primaryKey :: Boolean
  , autoIncrement :: Boolean
  , field :: String
  , references :: { model :: Model, key :: String | references }
  )

foreign import data Field :: Type

makeField :: forall o refsO value
           . Subrow o (DefineFieldParamsO value refsO)
          => Subrow refsO ReferencesParamsO
          => DefineFieldParams o -> Field
makeField = unsafeCoerce

foreign import data Model :: Type

foreign import defineImpl :: forall eff fields
                           . EffFn3 (sequelize :: SEQUELIZE | eff)
                               Sequelize String { | fields } Model

define :: forall eff fields
        . Sequelize
       -> String
       -> { | fields }
       -> Eff (sequelize :: SEQUELIZE | eff) Model
define = runEffFn3 defineImpl


foreign import data Instance :: Type

foreign import findByIdImpl :: forall eff
                             . EffFn4 (sequelize :: SEQUELIZE | eff)
                                 (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                 (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable Instance) Unit)
                                 Model Int Unit

findById :: forall eff
          . Model -> Int -> Aff (sequelize :: SEQUELIZE | eff) (Maybe Instance)
findById m i = makeAff \onError onSuccess -> runEffFn4 findByIdImpl (mkEffFn1 onError) (mkEffFn1 $ onSuccess <<< toMaybe) m i

type FindParams whereFields =
  { where :: { | whereFields }
  }

foreign import findOneImpl :: forall eff whereFields
                            . EffFn4 (sequelize :: SEQUELIZE | eff)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable Instance) Unit)
                                Model (FindParams whereFields) Unit

findOne :: forall eff whereFields
         . Model -> FindParams whereFields -> Aff (sequelize :: SEQUELIZE | eff) (Maybe Instance)
findOne m f = makeAff \onError onSuccess -> runEffFn4 findOneImpl (mkEffFn1 onError) (mkEffFn1 $ onSuccess <<< toMaybe) m f

foreign import findAllImpl :: forall eff whereFields
                            . EffFn4 (sequelize :: SEQUELIZE | eff)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) (Array Instance) Unit)
                                Model (FindParams whereFields) Unit

findAll :: forall eff whereFields
         . Model -> FindParams whereFields -> Aff (sequelize :: SEQUELIZE | eff) (Array Instance)
findAll m f = makeAff \onError onSuccess -> runEffFn4 findAllImpl (mkEffFn1 onError) (mkEffFn1 onSuccess) m f

foreign import data UnsavedInstance :: Type

foreign import buildImpl :: forall eff fields
                          . EffFn2 (sequelize :: SEQUELIZE | eff)
                              Model { | fields } UnsavedInstance

build :: forall eff fields
       . Model -> { | fields } -> Eff (sequelize :: SEQUELIZE | eff) UnsavedInstance
build = runEffFn2 buildImpl

foreign import saveImpl :: forall eff
                         . EffFn3 (sequelize :: SEQUELIZE | eff)
                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                             (EffFn1 (sequelize :: SEQUELIZE | eff) Instance Unit)
                             UnsavedInstance Unit

save :: forall eff
      . UnsavedInstance -> Aff (sequelize :: SEQUELIZE | eff) Instance
save i = makeAff \onError onSuccess -> runEffFn3 saveImpl (mkEffFn1 onError) (mkEffFn1 onSuccess) i

foreign import createImpl :: forall eff fields
                           . EffFn4 (sequelize :: SEQUELIZE | eff)
                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                               (EffFn1 (sequelize :: SEQUELIZE | eff) Instance Unit)
                               Model { | fields } Unit

create :: forall eff fields
        . Model -> { | fields } -> Aff (sequelize :: SEQUELIZE | eff) Instance
create m f = makeAff \onError onSuccess -> runEffFn4 createImpl (mkEffFn1 onError) (mkEffFn1 onSuccess) m f

foreign import bulkCreateImpl :: forall eff fields
                               . EffFn4 (sequelize :: SEQUELIZE | eff)
                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                   Model (Array { | fields }) Unit

bulkCreate :: forall eff fields
            . Model -> Array { | fields } -> Aff (sequelize :: SEQUELIZE | eff) Unit
bulkCreate m fs = makeAff \onError onSuccess -> runEffFn4 bulkCreateImpl (mkEffFn1 onError) (onSuccess unit) m fs

foreign import updateImpl :: forall eff fields
                           . EffFn4 (sequelize :: SEQUELIZE | eff)
                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                               (Eff (sequelize :: SEQUELIZE | eff) Unit)
                               Instance { | fields } Unit

update :: forall eff fields
        . Instance -> { | fields } -> Aff (sequelize :: SEQUELIZE | eff) Unit
update i f = makeAff \onError onSuccess -> runEffFn4 updateImpl (mkEffFn1 onError) (onSuccess unit) i f

-- foreign import staticUpdateImpl :: forall eff fields
--                                  . EffFn4 (sequelize :: SEQUELIZE | eff)
--                                      (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)

foreign import destroyImpl :: forall eff
                            . EffFn3 (sequelize :: SEQUELIZE | eff)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                Instance Unit

destroy :: forall eff
         . Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
destroy i = makeAff \onError onSuccess -> runEffFn3 destroyImpl (mkEffFn1 onError) (onSuccess unit) i

foreign import getImpl :: forall eff
                        . EffFn2 (sequelize :: SEQUELIZE | eff)
                            Instance { plain :: Boolean } Json

get :: forall eff
     . Instance -> { plain :: Boolean } -> Eff (sequelize :: SEQUELIZE | eff) Json
get = runEffFn2 getImpl
