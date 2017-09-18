module Database.Sequelize
  ( SEQUELIZE, Sequelize
  , SequelizeParams, Dialect (..)
  , sequelize, authenticate, sync
  , SequelizeType, sqlSTRING, sqlTEXT, sqlBOOLEAN, sqlDATE, sqlINTEGER, sqlFLOAT, sqlDOUBLE
  , SequelizeValue, sqlNOW
  , DefineFieldParams, DefineFieldParamsO, ReferencesParamsO
  , References, ReferencesParamsO, ReferencesParams, makeReferences
  , SequelizeDefer, sqlPgINITIALLYIMMEDIATE
  , Field, makeField
  , Model (..), ModelImpl, define, hasOne, belongsTo, hasMany, belongsToMany
  , Instance, findById, findOne, findAll
  , UnsavedInstance, build, save
  , create, bulkCreate, update, destroy, get
  ) where

import Prelude
import Data.Record.Class (class Subrow)
import Data.Nullable (Nullable, toNullable, toMaybe)
import Data.URI (Host)
import Data.URI.Host as Host
import Data.Maybe (Maybe (..))
import Data.Date (Date)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
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


type SequelizeImplParams =
  { database :: String
  , username :: String
  , password :: String
  , host     :: String
  , dialect  :: String
  , pool     :: { min :: Int, max :: Int, idle :: Int }
  }

-- FIXME needs to account for storage
type SequelizeParams =
  { database :: String
  , username :: String
  , password :: String
  , host     :: Host
  , dialect  :: Dialect
  , pool     :: { min :: Int, max :: Int, idle :: Int }
  }

toSequelizeImplParams :: SequelizeParams -> SequelizeImplParams
toSequelizeImplParams {database,username,password,host,dialect,pool} =
  { database,username,password,pool
  , host: Host.print host
  , dialect: show dialect
  }

-- FIXME
-- type SequelizeParamsO =
--   ( storage :: String -- Sqlite
--   )


foreign import data SEQUELIZE :: Effect

foreign import data Sequelize :: Type

foreign import sequelizeImpl :: forall eff o
                              -- . Subrow o SequelizeParamsO
                              . EffFn1 (sequelize :: SEQUELIZE | eff) SequelizeImplParams Sequelize


sequelize :: forall eff
           . SequelizeParams -> Eff (sequelize :: SEQUELIZE | eff) Sequelize
sequelize x = runEffFn1 sequelizeImpl (toSequelizeImplParams x)


foreign import authenticateImpl :: forall eff
                                 . EffFn3 (sequelize :: SEQUELIZE | eff)
                                     (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                     (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                     Sequelize Unit

authenticate :: forall eff. Sequelize -> Aff (sequelize :: SEQUELIZE | eff) Unit
authenticate sql = makeAff \onError onSuccess -> runEffFn3 authenticateImpl (mkEffFn1 onError) (onSuccess unit) sql


foreign import syncImpl :: forall eff
                         . EffFn3 (sequelize :: SEQUELIZE | eff)
                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                             Sequelize Unit


sync :: forall eff. Sequelize -> Aff (sequelize :: SEQUELIZE | eff) Unit
sync sql = makeAff \onError onSuccess -> runEffFn3 syncImpl (mkEffFn1 onError) (onSuccess unit) sql


foreign import data SequelizeType :: Type -> Type

foreign import sqlSTRING :: SequelizeType String
foreign import sqlTEXT :: SequelizeType String
foreign import sqlBOOLEAN :: SequelizeType Boolean
foreign import sqlDATE :: SequelizeType (SequelizeValue Date)
foreign import sqlINTEGER :: SequelizeType Int
foreign import sqlFLOAT :: SequelizeType Number
foreign import sqlDOUBLE :: SequelizeType Number

foreign import data SequelizeValue :: Type -> Type

foreign import sqlNOW :: SequelizeValue Date


type DefineFieldParams value o =
  { "type" :: SequelizeType value
  | o }

foreign import data SequelizeDefer :: Type

foreign import sqlPgINITIALLYIMMEDIATE :: SequelizeDefer

type ReferencesParamsO =
  ( deferrable :: SequelizeDefer
  )

type ReferencesParams o =
  { model :: ModelImpl
  , key :: String
  | o }

foreign import data References :: Type

makeReferences :: forall o
                . Subrow o ReferencesParamsO
               => ReferencesParams o -> References
makeReferences = unsafeCoerce

type DefineFieldParamsO value =
  ( allowNull :: Boolean
  , defaultValue :: value
  , unique :: String
  , primaryKey :: Boolean
  , autoIncrement :: Boolean
  , field :: String
  , references :: References
  )

foreign import data Field :: Type

makeField :: forall o value
           . Subrow o (DefineFieldParamsO value)
          => DefineFieldParams value o -> Field
makeField = unsafeCoerce

foreign import data ModelImpl :: Type

foreign import defineImpl :: forall eff fields
                           . EffFn3 (sequelize :: SEQUELIZE | eff)
                               Sequelize String { | fields } ModelImpl

data Model = Model String ModelImpl


define :: forall eff fields
        . Sequelize
       -> String
       -> { | fields }
       -> Eff (sequelize :: SEQUELIZE | eff) Model
define s n fs = Model n <$> runEffFn3 defineImpl s n fs


foreign import belongsToImpl :: forall eff
                              . EffFn3 (sequelize :: SEQUELIZE | eff)
                                  String ModelImpl ModelImpl
                                    { get :: EffFn2 (sequelize :: SEQUELIZE | eff)
                                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                               (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable Instance) Unit)
                                               Unit
                                    , set :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                               (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                               (Nullable Instance) Unit
                                    }

belongsTo :: forall eff
           . Model -> Model
          -> Aff (sequelize :: SEQUELIZE | eff)
               { get :: Aff (sequelize :: SEQUELIZE | eff) (Maybe Instance)
               , set :: Maybe Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
               }
belongsTo (Model childName childM) (Model _ parentM) = do
  {get,set} <- liftEff $ runEffFn3 belongsToImpl childName childM parentM
  pure
    { get: makeAff \onError onSuccess -> runEffFn2 get (mkEffFn1 onError) (mkEffFn1 \x -> onSuccess (toMaybe x))
    , set: \i -> makeAff \onError onSuccess -> runEffFn3 set (mkEffFn1 onError) (onSuccess unit) (toNullable i)
    }


foreign import hasOneImpl :: forall eff
                           . EffFn3 (sequelize :: SEQUELIZE | eff)
                               ModelImpl ModelImpl String
                                 { get :: EffFn2 (sequelize :: SEQUELIZE | eff)
                                            (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                            (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable Instance) Unit)
                                            Unit
                                 , set :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                            (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                            (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                            (Nullable Instance) Unit
                                 }

hasOne :: forall eff
        . Model -> Model
       -> Aff (sequelize :: SEQUELIZE | eff)
            { get :: Aff (sequelize :: SEQUELIZE | eff) (Maybe Instance)
            , set :: Maybe Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
            }
hasOne (Model _ parentM) (Model childName childM) = do
  {get,set} <- liftEff $ runEffFn3 hasOneImpl parentM childM childName
  pure
    { get: makeAff \onError onSuccess -> runEffFn2 get (mkEffFn1 onError) (mkEffFn1 $ \i -> onSuccess (toMaybe i))
    , set: \i -> makeAff \onError onSuccess -> runEffFn3 set (mkEffFn1 onError) (onSuccess unit) (toNullable i)
    }


foreign import hasManyImpl :: forall eff
                            . EffFn3 (sequelize :: SEQUELIZE | eff)
                                ModelImpl ModelImpl String
                                  { get :: EffFn2 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) (Array Instance) Unit)
                                             Unit
                                  , set :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                             (Array Instance) Unit
                                  , add :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                             (Array Instance) Unit
                                  , has :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Boolean Unit)
                                             (Array Instance) Unit
                                  , remove :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                             (Array Instance) Unit
                                  }

hasMany :: forall eff
         . Model -> Model
        -> Aff (sequelize :: SEQUELIZE | eff)
             { get :: Aff (sequelize :: SEQUELIZE | eff) (Array Instance)
             , set :: Array Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
             , add :: Array Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
             , has :: Array Instance -> Aff (sequelize :: SEQUELIZE | eff) Boolean
             , remove :: Array Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
             }
hasMany (Model _ parentM) (Model childName childM) = do
  {get,set,add,has,remove} <- liftEff $ runEffFn3 hasManyImpl parentM childM childName
  pure
    { get: makeAff \onError onSuccess -> runEffFn2 get (mkEffFn1 onError) (mkEffFn1 onSuccess)
    , set: \is -> makeAff \onError onSuccess -> runEffFn3 set (mkEffFn1 onError) (onSuccess unit) is
    , add: \is -> makeAff \onError onSuccess -> runEffFn3 add (mkEffFn1 onError) (onSuccess unit) is
    , has: \is -> makeAff \onError onSuccess -> runEffFn3 has (mkEffFn1 onError) (mkEffFn1 onSuccess) is
    , remove: \is -> makeAff \onError onSuccess -> runEffFn3 remove (mkEffFn1 onError) (onSuccess unit) is
    }


foreign import belongsToManyImpl :: forall eff fields
                                  . EffFn4 (sequelize :: SEQUELIZE | eff)
                                      String ModelImpl ModelImpl { through :: Model }
                                        { get :: EffFn2 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) (Array Instance) Unit)
                                                   Unit
                                        , set :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                                   (Array Instance) { through :: { | fields } } Unit
                                        , add :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                                   (Array Instance) { through :: { | fields } } Unit
                                        , has :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Boolean Unit)
                                                   (Array Instance) Unit
                                        , remove :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                                   (Array Instance) Unit
                                        }

belongsToMany :: forall eff fields
               . Model -> Model -> {through :: Model}
              -> Aff (sequelize :: SEQUELIZE | eff)
                   { get :: Aff (sequelize :: SEQUELIZE | eff) (Array Instance)
                   , set :: Array Instance -> { through :: { | fields } } -> Aff (sequelize :: SEQUELIZE | eff) Unit
                   , add :: Array Instance -> { through :: { | fields } } -> Aff (sequelize :: SEQUELIZE | eff) Unit
                   , has :: Array Instance -> Aff (sequelize :: SEQUELIZE | eff) Boolean
                   , remove :: Array Instance -> Aff (sequelize :: SEQUELIZE | eff) Unit
                   }
belongsToMany (Model childName childM) (Model _ parentM) through = do
  {get,set,add,has,remove} <- liftEff $ runEffFn4 belongsToManyImpl childName childM parentM through
  pure
    { get: makeAff \onError onSuccess -> runEffFn2 get (mkEffFn1 onError) (mkEffFn1 onSuccess)
    , set: \is th -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) is th
    , add: \is th -> makeAff \onError onSuccess -> runEffFn4 add (mkEffFn1 onError) (onSuccess unit) is th
    , has: \is -> makeAff \onError onSuccess -> runEffFn3 has (mkEffFn1 onError) (mkEffFn1 onSuccess) is
    , remove: \is -> makeAff \onError onSuccess -> runEffFn3 remove (mkEffFn1 onError) (onSuccess unit) is
    }


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

foreign import getImpl :: forall eff fields
                        . EffFn2 (sequelize :: SEQUELIZE | eff)
                            Instance { plain :: Boolean } { | fields }

get :: forall eff fields
     . Instance -> { plain :: Boolean } -> Eff (sequelize :: SEQUELIZE | eff) { | fields }
get = runEffFn2 getImpl
