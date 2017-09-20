module Database.Sequelize
  ( SEQUELIZE, Sequelize
  , SequelizeParams, Dialect (..)
  , sequelize, authenticate, sync
  , Model (..), define, hasOne, belongsTo, hasMany, belongsToMany
  , Instance, findById, findOne, findAll
  , UnsavedInstance, build, save
  , create, bulkCreate, update, destroy, get
  , module Fields
  ) where

import Database.Sequelize.Fields (ModelDefinition, ModelImpl)
import Database.Sequelize.Fields as Fields

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
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1, runEffFn1, EffFn2, runEffFn2, EffFn3, runEffFn3, EffFn4, runEffFn4, EffFn5, runEffFn5)
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



foreign import defineImpl :: forall eff fields constructor
                           . EffFn3 (sequelize :: SEQUELIZE | eff)
                               Sequelize String (ModelDefinition fields constructor) (ModelImpl fields constructor)

data Model fields constructor = Model String (ModelImpl fields constructor)


define :: forall eff fields constructor
        . Sequelize
       -> String
       -> ModelDefinition fields constructor
       -> Eff (sequelize :: SEQUELIZE | eff) (Model fields constructor)
define s n fs = Model n <$> runEffFn3 defineImpl s n fs


type BelongsToParamsO =
  ( hooks :: Boolean
  , as :: String
  , foreignKey :: String
  , targetKey :: String
  , onDelete :: String
  , onUpdate :: String
  , constraints :: Boolean
  )


foreign import belongsToImpl :: forall eff childFields childConstructor parentFields parentConstructor params
                              . Subrow params BelongsToParamsO
                             => EffFn4 (sequelize :: SEQUELIZE | eff)
                                  String
                                  (ModelImpl childFields childConstructor)
                                  (ModelImpl parentFields parentConstructor)
                                  { | params }
                                    { get :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                               (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable (Instance childFields)) Unit)
                                               (Instance parentFields)
                                               Unit
                                    , set :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                               (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                               (Instance parentFields)
                                               (Nullable (Instance childFields)) Unit
                                    }

belongsTo :: forall eff childFields childConstructor parentFields parentConstructor params
           . Subrow params BelongsToParamsO
          => Model childFields childConstructor -> Model parentFields parentConstructor -> { | params }
          -> Aff (sequelize :: SEQUELIZE | eff)
               { get :: Instance parentFields -> Aff (sequelize :: SEQUELIZE | eff) (Maybe (Instance childFields))
               , set :: Instance parentFields -> Maybe (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Unit
               }
belongsTo (Model childName childM) (Model _ parentM) ps = do
  {get,set} <- liftEff $ runEffFn4 belongsToImpl childName childM parentM ps
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 \x -> onSuccess (toMaybe x)) q
    , set: \q i -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) q (toNullable i)
    }



type HasOneParamsO =
  ( hooks :: Boolean
  , as :: String
  , foreignKey :: String
  , onDelete :: String
  , onUpdate :: String
  , constraints :: Boolean
  )



foreign import hasOneImpl :: forall eff childFields childConstructor parentFields parentConstructor params
                           . Subrow params HasOneParamsO
                          => EffFn4 (sequelize :: SEQUELIZE | eff)
                               (ModelImpl parentFields parentConstructor)
                               (ModelImpl childFields childConstructor) String
                               { | params }
                                 { get :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                            (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                            (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable (Instance childFields)) Unit)
                                            (Instance parentFields)
                                            Unit
                                 , set :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                            (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                            (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                            (Instance parentFields)
                                            (Nullable (Instance childFields)) Unit
                                 }

hasOne :: forall eff childFields childConstructor parentFields parentConstructor params
        . Subrow params HasOneParamsO
       => Model parentFields parentConstructor
       -> Model childFields childConstructor
       -> { | params }
       -> Aff (sequelize :: SEQUELIZE | eff)
            { get :: Instance parentFields -> Aff (sequelize :: SEQUELIZE | eff) (Maybe (Instance childFields))
            , set :: Instance parentFields -> Maybe (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Unit
            }
hasOne (Model _ parentM) (Model childName childM) ps = do
  {get,set} <- liftEff $ runEffFn4 hasOneImpl parentM childM childName ps
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 $ \i -> onSuccess (toMaybe i)) q
    , set: \q i -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) q (toNullable i)
    }


type HasManyParamsO =
  ( hooks :: Boolean
  , as :: String
  , foreignKey :: String
  , sourceKey :: String
  , onDelete :: String
  , onUpdate :: String
  , constraints :: Boolean
  )


foreign import hasManyImpl :: forall eff childFields childConstructor parentFields parentConstructor params
                            . Subrow params HasManyParamsO
                           => EffFn4 (sequelize :: SEQUELIZE | eff)
                                (ModelImpl parentFields parentConstructor)
                                (ModelImpl childFields childConstructor) String
                                { | params }
                                  { get :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) (Array (Instance childFields)) Unit)
                                             (Instance parentFields)
                                             Unit
                                  , set :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                             (Instance parentFields)
                                             (Array (Instance childFields)) Unit
                                  , add :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                             (Instance parentFields)
                                             (Array (Instance childFields)) Unit
                                  , has :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Boolean Unit)
                                             (Instance parentFields)
                                             (Array (Instance childFields)) Unit
                                  , remove :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                             (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                             (Instance parentFields)
                                             (Array (Instance childFields)) Unit
                                  }

hasMany :: forall eff childFields childConstructor parentFields parentConstructor params
         . Subrow params HasManyParamsO
        => Model parentFields parentConstructor -> Model childFields childConstructor -> { | params }
        -> Aff (sequelize :: SEQUELIZE | eff)
             { get :: Instance parentFields -> Aff (sequelize :: SEQUELIZE | eff) (Array (Instance childFields))
             , set :: Instance parentFields -> Array (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Unit
             , add :: Instance parentFields -> Array (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Unit
             , has :: Instance parentFields -> Array (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Boolean
             , remove :: Instance parentFields -> Array (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Unit
             }
hasMany (Model _ parentM) (Model childName childM) ps = do
  {get,set,add,has,remove} <- liftEff $ runEffFn4 hasManyImpl parentM childM childName ps
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 onSuccess) q
    , set: \q is -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) q is
    , add: \q is -> makeAff \onError onSuccess -> runEffFn4 add (mkEffFn1 onError) (onSuccess unit) q is
    , has: \q is -> makeAff \onError onSuccess -> runEffFn4 has (mkEffFn1 onError) (mkEffFn1 onSuccess) q is
    , remove: \q is -> makeAff \onError onSuccess -> runEffFn4 remove (mkEffFn1 onError) (onSuccess unit) q is
    }


type BelongsToManyParams throughFields throughConstructor o =
  { through ::
    { model :: ModelImpl throughFields throughConstructor
    , unique :: Boolean
    }
  | o }

type BelongsToManyParamsO =
  ( hooks :: Boolean
  , as :: String
  , foreignKey :: String
  , otherKey :: String
  , timestamps :: Boolean
  , onDelete :: String
  , onUpdate :: String
  , constraints :: Boolean
  )


foreign import belongsToManyImpl :: forall eff fields childFields childConstructor parentFields parentConstructor throughFields throughConstructor params
                                  . Subrow params BelongsToManyParamsO
                                 => EffFn4 (sequelize :: SEQUELIZE | eff)
                                      String (ModelImpl childFields childConstructor)
                                      (ModelImpl parentFields parentConstructor)
                                      (BelongsToManyParams throughFields throughConstructor params)
                                        { get :: EffFn3 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) (Array (Instance childFields)) Unit)
                                                   (Instance parentFields)
                                                   Unit
                                        , set :: EffFn5 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                                   (Instance parentFields)
                                                   (Array (Instance childFields))
                                                   { through :: { | throughConstructor } }
                                                   Unit
                                        , add :: EffFn5 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                                   (Instance parentFields)
                                                   (Array (Instance childFields))
                                                   { through :: { | throughConstructor } }
                                                   Unit
                                        , has :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Boolean Unit)
                                                   (Instance parentFields)
                                                   (Array (Instance childFields)) Unit
                                        , remove :: EffFn4 (sequelize :: SEQUELIZE | eff)
                                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                                   (Instance parentFields)
                                                   (Array (Instance childFields)) Unit
                                        }

belongsToMany :: forall eff fields childFields childConstructor parentFields parentConstructor throughFields throughConstructor params
               . Subrow params BelongsToManyParamsO
              => Model childFields childConstructor
              -> Model parentFields parentConstructor
              -> (BelongsToManyParams throughFields throughConstructor params)
              -> Aff (sequelize :: SEQUELIZE | eff)
                   { get :: Instance parentFields -> Aff (sequelize :: SEQUELIZE | eff) (Array (Instance childFields))
                   , set :: Instance parentFields -> Array (Instance childFields) -> { through :: { | throughConstructor } } -> Aff (sequelize :: SEQUELIZE | eff) Unit
                   , add :: Instance parentFields -> Array (Instance childFields) -> { through :: { | throughConstructor } } -> Aff (sequelize :: SEQUELIZE | eff) Unit
                   , has :: Instance parentFields -> Array (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Boolean
                   , remove :: Instance parentFields -> Array (Instance childFields) -> Aff (sequelize :: SEQUELIZE | eff) Unit
                   }
belongsToMany (Model childName childM) (Model _ parentM) through = do
  {get,set,add,has,remove} <- liftEff $ runEffFn4 belongsToManyImpl childName childM parentM through
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 onSuccess) q
    , set: \q is th -> makeAff \onError onSuccess -> runEffFn5 set (mkEffFn1 onError) (onSuccess unit) q is th
    , add: \q is th -> makeAff \onError onSuccess -> runEffFn5 add (mkEffFn1 onError) (onSuccess unit) q is th
    , has: \q is -> makeAff \onError onSuccess -> runEffFn4 has (mkEffFn1 onError) (mkEffFn1 onSuccess) q is
    , remove: \q is -> makeAff \onError onSuccess -> runEffFn4 remove (mkEffFn1 onError) (onSuccess unit) q is
    }


foreign import data Instance :: # Type -> Type

foreign import findByIdImpl :: forall eff fields constructor
                             . EffFn4 (sequelize :: SEQUELIZE | eff)
                                 (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                 (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable (Instance fields)) Unit)
                                 (ModelImpl fields constructor) Int Unit

findById :: forall eff fields constructor
          . Model fields constructor
         -> Int -> Aff (sequelize :: SEQUELIZE | eff) (Maybe (Instance fields))
findById (Model _ m) i = makeAff \onError onSuccess -> runEffFn4 findByIdImpl (mkEffFn1 onError) (mkEffFn1 $ onSuccess <<< toMaybe) m i

type FindParams whereFields =
  { where :: { | whereFields }
  }

foreign import findOneImpl :: forall eff whereFields fields constructor
                            . EffFn4 (sequelize :: SEQUELIZE | eff)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) (Nullable (Instance fields)) Unit)
                                (ModelImpl fields constructor) (FindParams whereFields) Unit

findOne :: forall eff whereFields fields constructor
         . Model fields constructor
        -> FindParams whereFields
        -> Aff (sequelize :: SEQUELIZE | eff) (Maybe (Instance fields))
findOne (Model _ m) f = makeAff \onError onSuccess -> runEffFn4 findOneImpl (mkEffFn1 onError) (mkEffFn1 $ onSuccess <<< toMaybe) m f

foreign import findAllImpl :: forall eff whereFields fields constructor
                            . EffFn4 (sequelize :: SEQUELIZE | eff)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) (Array (Instance fields)) Unit)
                                (ModelImpl fields constructor) (FindParams whereFields) Unit

findAll :: forall eff whereFields fields constructor
         . Model fields constructor
        -> FindParams whereFields
        -> Aff (sequelize :: SEQUELIZE | eff) (Array (Instance fields))
findAll (Model _ m) f = makeAff \onError onSuccess -> runEffFn4 findAllImpl (mkEffFn1 onError) (mkEffFn1 onSuccess) m f

foreign import data UnsavedInstance :: # Type -> Type

foreign import buildImpl :: forall eff fields constructor
                          . EffFn2 (sequelize :: SEQUELIZE | eff)
                              (ModelImpl fields constructor)
                              { | constructor }
                              (UnsavedInstance fields)

build :: forall eff fields constructor
       . Model fields constructor
      -> { | constructor } -> Eff (sequelize :: SEQUELIZE | eff) (UnsavedInstance fields)
build (Model _ m) f = runEffFn2 buildImpl m f

foreign import saveImpl :: forall eff fields
                         . EffFn3 (sequelize :: SEQUELIZE | eff)
                             (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                             (EffFn1 (sequelize :: SEQUELIZE | eff) (Instance fields) Unit)
                             (UnsavedInstance fields) Unit

save :: forall eff fields
      . UnsavedInstance fields -> Aff (sequelize :: SEQUELIZE | eff) (Instance fields)
save i = makeAff \onError onSuccess -> runEffFn3 saveImpl (mkEffFn1 onError) (mkEffFn1 onSuccess) i

foreign import createImpl :: forall eff fields constructor
                           . EffFn4 (sequelize :: SEQUELIZE | eff)
                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                               (EffFn1 (sequelize :: SEQUELIZE | eff) (Instance fields) Unit)
                               (ModelImpl fields constructor)
                               { | constructor } Unit

create :: forall eff fields constructor
        . Model fields constructor
       -> { | constructor } -> Aff (sequelize :: SEQUELIZE | eff) (Instance fields)
create (Model _ m) f = makeAff \onError onSuccess -> runEffFn4 createImpl (mkEffFn1 onError) (mkEffFn1 onSuccess) m f

foreign import bulkCreateImpl :: forall eff fields constructor
                               . EffFn4 (sequelize :: SEQUELIZE | eff)
                                   (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                   (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                   (ModelImpl fields constructor)
                                   (Array { | constructor }) Unit

bulkCreate :: forall eff fields constructor
            . Model fields constructor
           -> Array { | constructor } -> Aff (sequelize :: SEQUELIZE | eff) Unit
bulkCreate (Model _ m) fs = makeAff \onError onSuccess -> runEffFn4 bulkCreateImpl (mkEffFn1 onError) (onSuccess unit) m fs

foreign import updateImpl :: forall eff fields fields'
                           . EffFn4 (sequelize :: SEQUELIZE | eff)
                               (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                               (Eff (sequelize :: SEQUELIZE | eff) Unit)
                               (Instance fields) { | fields' } Unit

update :: forall eff fields fields'
        . Instance fields -> { | fields' } -> Aff (sequelize :: SEQUELIZE | eff) Unit
update i f = makeAff \onError onSuccess -> runEffFn4 updateImpl (mkEffFn1 onError) (onSuccess unit) i f

-- foreign import staticUpdateImpl :: forall eff fields
--                                  . EffFn4 (sequelize :: SEQUELIZE | eff)
--                                      (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)

foreign import destroyImpl :: forall eff fields
                            . EffFn3 (sequelize :: SEQUELIZE | eff)
                                (EffFn1 (sequelize :: SEQUELIZE | eff) Error Unit)
                                (Eff (sequelize :: SEQUELIZE | eff) Unit)
                                (Instance fields) Unit

destroy :: forall eff fields
         . Instance fields -> Aff (sequelize :: SEQUELIZE | eff) Unit
destroy i = makeAff \onError onSuccess -> runEffFn3 destroyImpl (mkEffFn1 onError) (onSuccess unit) i

foreign import getImpl :: forall eff fields
                        . EffFn2 (sequelize :: SEQUELIZE | eff)
                            (Instance fields) { plain :: Boolean } { | fields }

get :: forall eff fields
     . Instance fields
    -> { plain :: Boolean } -> Eff (sequelize :: SEQUELIZE | eff) { | fields }
get = runEffFn2 getImpl
