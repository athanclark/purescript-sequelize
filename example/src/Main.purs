module Main where

import Data.URI (Host (..))
import Database.Sequelize (sequelize, Dialect (Postgres), authenticate, sync, define, sqlSTRING, sqlINTEGER, build, save, makeField, get, findOne)

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow, logShow)
import Unsafe.Coerce (unsafeCoerce)


type Fields =
  { foo :: String
  , bar :: Int
  }

type FieldsDefaults =
  { bar :: Int
  }

main :: Eff _ Unit
main = do
  log "Hello sailor!"
  let args =
        { database: "athan"
        , username: "athan"
        , password: "foo"
        , host: NameAddress "localhost"
        , dialect: Postgres
        , pool: {min: 0, max: 5, idle: 10000}
        }
  sql <- sequelize args
  void $ runAff errorShow logShow $ do
    authenticate sql
    m <- liftEff $ define sql "foo"
      { foo: makeField
          { "type": sqlSTRING
          , defaultValue: "foo!"
          }
      , bar: makeField
          { "type": sqlINTEGER
          }
      }
    sync sql
    -- u <- liftEff $ build m {bar: 1}
    -- i <- save u
    mI <- findOne m {where: {bar: 1}}
    case mI of
      Nothing -> do
        liftEff $ log "wut?!"
        pure "wut"
      Just i -> do
        x <- liftEff $ get i {plain: true}
        pure (x :: Fields).foo
