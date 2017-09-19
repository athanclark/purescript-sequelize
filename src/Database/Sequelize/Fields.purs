module Database.Sequelize.Fields
  ( ModelDefinition, ModelImpl
  , SequelizeType, sqlSTRING, sqlCHAR, sqlTEXT, sqlBOOLEAN, sqlBLOB, sqlDATE, sqlINTEGER, sqlBIGINT, sqlFLOAT, sqlDOUBLE, sqlDECIMAL, sqlREAL, sqlUUID, sqlJSON, sqlARRAY
  , sqlNOW, UUID (..), sqlUUIDV1, sqlUUIDV4, SequelizeDefer, sqlPgINITIALLYIMMEDIATE
  , ReferencesParamsO, ReferencesParams, References, makeReferences
  , DefineFieldParamsO, DefineFieldParams, addField, addFieldWithDefault, emptyModelDefinition
  ) where


import Data.JSDate (JSDate)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Data.Record.Class (class Subrow)
import Data.Symbol (SProxy, reflectSymbol, class IsSymbol)
import Data.Argonaut (Json)
import Node.Buffer (Buffer)
import Unsafe.Coerce



foreign import data ModelImpl :: # Type -> # Type -> Type


foreign import data SequelizeType :: Type -> Type

foreign import sqlSTRING :: SequelizeType String
foreign import sqlCHAR :: SequelizeType String
foreign import sqlTEXT :: SequelizeType String
foreign import sqlBOOLEAN :: SequelizeType Boolean
foreign import sqlBLOB :: SequelizeType Buffer
foreign import sqlDATE :: SequelizeType JSDate
foreign import sqlINTEGER :: SequelizeType Int
foreign import sqlBIGINT :: SequelizeType Int
foreign import sqlFLOAT :: SequelizeType Number
foreign import sqlDOUBLE :: SequelizeType Number
foreign import sqlDECIMAL :: SequelizeType Number
foreign import sqlREAL :: SequelizeType Number
foreign import sqlUUID :: SequelizeType UUID
foreign import sqlJSON :: SequelizeType Json
foreign import sqlARRAY :: forall a. SequelizeType a -> SequelizeType (Array a)
foreign import sqlENUM :: Array String -> SequelizeType String

foreign import sqlNOW :: JSDate
foreign import sqlUUIDV1 :: UUID
foreign import sqlUUIDV4 :: UUID


newtype UUID = UUID String


foreign import data SequelizeDefer :: Type

foreign import sqlPgINITIALLYIMMEDIATE :: SequelizeDefer

type ReferencesParamsO =
  ( deferrable :: SequelizeDefer
  )

type ReferencesParams o modelFields modelConstructor =
  { model :: ModelImpl modelFields modelConstructor
  , key :: String
  | o }

foreign import data References :: Type

makeReferences :: forall o modelFields modelConstructor
                . Subrow o ReferencesParamsO
               => ReferencesParams o modelFields modelConstructor -> References
makeReferences = unsafeCoerce

type DefineFieldParams value o =
  { "type" :: SequelizeType value
  | o }

type DefineFieldParamsO =
  ( allowNull :: Boolean
  , unique :: String
  , primaryKey :: Boolean
  , autoIncrement :: Boolean
  , field :: String
  , references :: References
  )


addField :: forall o k value fields fields' constructor constructor'
          . Subrow o DefineFieldParamsO
         => IsSymbol k
         => RowCons k value fields fields'
         => RowCons k value constructor constructor'
         => SProxy k
         -> DefineFieldParams value o
         -> ModelDefinition fields  constructor
         -> ModelDefinition fields' constructor'
addField k q acc =
  runFn3 unsafeAddModelDef (reflectSymbol k) q acc

addFieldWithDefault :: forall o k value fields fields' constructor
                     . Subrow o DefineFieldParamsO
                    => IsSymbol k
                    => RowCons k value fields fields'
                    => SProxy k
                    -> DefineFieldParams value o
                    -> value
                    -> ModelDefinition fields  constructor
                    -> ModelDefinition fields' constructor
addFieldWithDefault k q d acc =
  runFn3 unsafeAddModelDef (reflectSymbol k) (runFn2 unsafeAddDefaultValue q d) acc

foreign import data ModelDefinition :: # Type -> # Type -> Type

foreign import emptyModelDefinition :: ModelDefinition () ()

foreign import unsafeAddModelDef :: forall q a1 a2 b1 b2. Fn3 String q (ModelDefinition a1 a2) (ModelDefinition b1 b2)

foreign import unsafeAddDefaultValue :: forall q a. Fn2 q a q
