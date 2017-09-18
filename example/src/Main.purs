module Main where

import Data.URI (Host (..))
import Database.Sequelize

import Prelude
import Data.Symbol (SProxy (..))
import Data.Maybe (Maybe (..))
import Data.JSDate (JSDate)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow, logShow)
import Unsafe.Coerce (unsafeCoerce)


type Fields =
  { foo :: String
  , bar :: Int
  , time :: JSDate
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
  void $ runAff errorShow log $ do
    authenticate sql
    foo <- liftEff $ define sql "foo"
             $ addField (SProxy :: SProxy "bar")
                { "type": sqlINTEGER
                }
             $ addFieldWithDefault (SProxy :: SProxy "foo")
                { "type": sqlSTRING
                } "foo!"
             $ addFieldWithDefault (SProxy :: SProxy "time")
                { "type": sqlDATE
                } sqlNOW
             $ emptyModelDefinition
    baz <- liftEff $ define sql "baz"
             $ addFieldWithDefault (SProxy :: SProxy "baz")
                { "type": sqlSTRING
                } "foo!"
             $ emptyModelDefinition
    foo'sBazs <- foo `hasMany` baz
    sync sql
    void $ create foo {bar: 1}
    b <- create baz {}
    mI <- findOne foo {where: {bar: 1}}
    case mI of
      Nothing -> do
        liftEff $ log "wut?!"
        pure "wut"
      Just i -> do
        foo'sBazs.set i [b]
        x <- liftEff $ get i {plain: true}
        pure $ unsafeCoerce x
