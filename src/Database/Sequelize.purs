module Database.Sequelize
  ( SEQUELIZE, Sequelize
  , SequelizeParams, Dialect (..)
  , sequelize, authenticate, sync
  , Model (..), define, hasOne, belongsTo, hasMany, belongsToMany
  , Instance, findById, findOne, findAll
  , UnsavedInstance, build, save
  , create, bulkCreate, update, destroy, get
  , module Fields
  , OneToOneResult, ManyToOneResult, ManyToManyResult
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
import Data.String.Inflection (capitalize)
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


foreign import belongsToImpl :: forall eff childFields childConstructor parentFields parentConstructor
                              . EffFn3 (sequelize :: SEQUELIZE | eff)
                                  String
                                  (ModelImpl childFields childConstructor)
                                  (ModelImpl parentFields parentConstructor)
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


type OneToOneResult eff parentFields childFields =
  { get :: Instance parentFields -> Aff eff (Maybe (Instance childFields))
  , set :: Instance parentFields -> Maybe (Instance childFields) -> Aff eff Unit
  }


belongsTo :: forall eff childFields childConstructor parentFields parentConstructor
           . Model childFields childConstructor -> Model parentFields parentConstructor
          -> Aff (sequelize :: SEQUELIZE | eff)
               (OneToOneResult (sequelize :: SEQUELIZE | eff) parentFields childFields)
belongsTo (Model childName childM) (Model _ parentM) = do
  {get,set} <- liftEff $ runEffFn3 belongsToImpl childName childM parentM
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 \x -> onSuccess (toMaybe x)) q
    , set: \q i -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) q (toNullable i)
    }


foreign import hasOneImpl :: forall eff childFields childConstructor parentFields parentConstructor
                           . EffFn3 (sequelize :: SEQUELIZE | eff)
                               (ModelImpl parentFields parentConstructor)
                               (ModelImpl childFields childConstructor) String
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

hasOne :: forall eff childFields childConstructor parentFields parentConstructor
        . Model parentFields parentConstructor
       -> Model childFields childConstructor
       -> Aff (sequelize :: SEQUELIZE | eff)
            (OneToOneResult (sequelize :: SEQUELIZE | eff) parentFields childFields)
hasOne (Model _ parentM) (Model childName childM) = do
  {get,set} <- liftEff $ runEffFn3 hasOneImpl parentM childM childName
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 $ \i -> onSuccess (toMaybe i)) q
    , set: \q i -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) q (toNullable i)
    }


foreign import hasManyImpl :: forall eff childFields childConstructor parentFields parentConstructor
                            . EffFn3 (sequelize :: SEQUELIZE | eff)
                                (ModelImpl parentFields parentConstructor)
                                (ModelImpl childFields childConstructor) String
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

type ManyToOneResult eff parentFields childFields =
  { get :: Instance parentFields -> Aff eff (Array (Instance childFields))
  , set :: Instance parentFields -> Array (Instance childFields) -> Aff eff Unit
  , add :: Instance parentFields -> Array (Instance childFields) -> Aff eff Unit
  , has :: Instance parentFields -> Array (Instance childFields) -> Aff eff Boolean
  , remove :: Instance parentFields -> Array (Instance childFields) -> Aff eff Unit
  }

hasMany :: forall eff childFields childConstructor parentFields parentConstructor
         . Model parentFields parentConstructor -> Model childFields childConstructor
        -> Aff (sequelize :: SEQUELIZE | eff)
             (ManyToOneResult (sequelize :: SEQUELIZE | eff) parentFields childFields)
hasMany (Model _ parentM) (Model childName childM) = do
  {get,set,add,has,remove} <- liftEff $ runEffFn3 hasManyImpl parentM childM childName
  pure
    { get: \q -> makeAff \onError onSuccess -> runEffFn3 get (mkEffFn1 onError) (mkEffFn1 onSuccess) q
    , set: \q is -> makeAff \onError onSuccess -> runEffFn4 set (mkEffFn1 onError) (onSuccess unit) q is
    , add: \q is -> makeAff \onError onSuccess -> runEffFn4 add (mkEffFn1 onError) (onSuccess unit) q is
    , has: \q is -> makeAff \onError onSuccess -> runEffFn4 has (mkEffFn1 onError) (mkEffFn1 onSuccess) q is
    , remove: \q is -> makeAff \onError onSuccess -> runEffFn4 remove (mkEffFn1 onError) (onSuccess unit) q is
    }


-- type BelongsToManyThroughO =
--   ( 
--   )


foreign import belongsToManyImpl :: forall eff fields childFields childConstructor parentFields parentConstructor throughFields throughConstructor
                                  . EffFn3 (sequelize :: SEQUELIZE | eff)
                                      String (ModelImpl childFields childConstructor)
                                      (ModelImpl parentFields parentConstructor)
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

type ManyToManyResult eff parentFields childFields throughConstructor =
  { get :: Instance parentFields -> Aff eff (Array (Instance childFields))
  , set :: Instance parentFields -> Array (Instance childFields) -> { through :: { | throughConstructor } } -> Aff eff Unit
  , add :: Instance parentFields -> Array (Instance childFields) -> { through :: { | throughConstructor } } -> Aff eff Unit
  , has :: Instance parentFields -> Array (Instance childFields) -> Aff eff Boolean
  , remove :: Instance parentFields -> Array (Instance childFields) -> Aff eff Unit
  }

belongsToMany :: forall eff fields childFields childConstructor parentFields parentConstructor throughConstructor
               . Model childFields childConstructor
              -> Model parentFields parentConstructor
              -> Aff (sequelize :: SEQUELIZE | eff)
                   (ManyToManyResult (sequelize :: SEQUELIZE | eff) parentFields childFields throughConstructor)
belongsToMany (Model childName childM) (Model parentName parentM) = do
  let throughName
        | childName < parentName = capitalize childName <> capitalize parentName
        | otherwise              = capitalize parentName <> capitalize childName
  {get,set,add,has,remove} <- liftEff $ runEffFn3 belongsToManyImpl throughName childM parentM
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
